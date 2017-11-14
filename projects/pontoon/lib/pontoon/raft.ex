defmodule Pontoon.Raft do
  require Logger
  use GenServer

  @rpc_port "9213"

  # Structs defining the format of RPC messages.
  defmodule RPC do
    @derive [Poison.Encoder]
    defstruct [:type, :name, :data]

    defmodule RequestVote do
      @derive [Poison.Encoder]
      defstruct [:term, :candidate, :last_log_idx, :last_log_term]
    end

    defmodule AppendEntries do
      @derive [Poison.Encoder]
      defstruct [:term, :leader, :prev_log_idx, :prev_log_term, :leader_commit, :entries]
    end

    defmodule ReplyRequestVote do
      @derive [Poison.Encoder]
      defstruct [:term, :granted]
    end

    defmodule ReplyAppendEntries do
      @derive [Poison.Encoder]
      defstruct [:term, :success]
    end

    # FIXME: This feels redundant and gross
    def decode!(raw) do
      rpc = Poison.decode!(raw, as: %RPC{})
      member = Pontoon.Membership.get_member(rpc.name)

      msg =
        case rpc.type do
          "AppendEntries" ->
            Poison.decode!(rpc.data, as: %AppendEntries{})
          "ReplyAppendEntries" ->
            Poison.decode!(rpc.data, as: %ReplyAppendEntries{})
          "RequestVote" ->
            Poison.decode!(rpc.data, as: %RequestVote{})
          "ReplyRequestVote" ->
            Poison.decode!(rpc.data, as: %ReplyRequestVote{})
          unknown         ->
            Logger.error("unknown RPC message type: #{inspect unknown}")
            nil
        end

      {msg, member}
    end

    def encode(message) do
      # FIXME: this sucks
      [name] = message.__struct__
      |> Module.split
      |> Enum.take(-1)

      encode(name, Poison.encode!(message))
    end

    def encode(type, data) do
      name = Pontoon.Membership.get_own_name()

      Poison.encode!(%RPC{type: type, data: data, name: name})
    end
  end

  defmodule State do
    @leader_election_interval_ms 4000

    defstruct [
      :role, :leader, :term, :log, :commit_log, :commit_idx,
      :election_timer, :leader_state, :voted_for
    ]

    defmodule Leader do
      defstruct [:votes, :match_idx, :next_idx]
    end

    def new() do
      %__MODULE__{role: :candidate, leader: nil, term: 0, log: [], commit_log: [], commit_idx: 0}
    end

    def schedule_leader_election(state, pid) do
      timer = Process.send_after(pid, :leader_election, @leader_election_interval_ms)

      # Clear any existing timer
      if state.election_timer do
        Process.cancel_timer(state.election_timer)
      end

      %{state | election_timer: timer}
    end

    def maybe_step_down(state, remote_term) do
      if state.term < remote_term do
        Logger.info("Remote node has higher term! Stepping down")
        %{state | term: remote_term, role: :follower, voted_for: nil}
      else
        state
      end
    end

    # AppendEntries RPC implementation.
    def handle_rpc(state, _member, %RPC.AppendEntries{} = rpc) do
      # TODO: Need to do some checks to see if we should accept this message
      state = maybe_step_down(state, rpc.term)

      {nil, state}
    end

    # RequestVote RPC implementation
    def handle_rpc(state, _member, %RPC.RequestVote{} = rpc) do
      state = maybe_step_down(state, rpc.term)

      vote_granted =
        cond do
          rpc.term < state.term ->
            false

          !is_nil(state.voted_for) && state.voted_for != rpc.candidate ->
            false

          # FIXME: idk if commit_idx is correct
          rpc.last_log_idx < state.commit_idx ->
            false

          # Everything checks out!
          true -> true
        end

      state =
        if vote_granted do
          %{state | voted_for: rpc.candidate}
        else
          state
        end

      reply = %RPC.ReplyRequestVote{term: state.term, granted: vote_granted}
      {reply, state}
    end

    def handle_rpc(state, _member, %RPC.ReplyRequestVote{} = rpc) do
      Logger.info("Got request vote reply: #{inspect rpc}")
      state = maybe_step_down(state, rpc.term)

      # TODO: Need to check that the request was actually granted and such

      {nil, state}
    end

    def handle_rpc(state, _member, %RPC.ReplyAppendEntries{} = rpc) do
      Logger.info("Got append entries reply: #{inspect rpc}")

      {nil, state}
    end
  end

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def init(:ok) do
    port = (System.get_env("RPC_PORT") || @rpc_port)
    |> String.to_integer

    Logger.info("Starting Raft on port #{inspect port}")
    {:ok, socket} = :gen_udp.open(port, [
          :binary,
          ip: {0, 0, 0, 0},
          active: true,
          reuseaddr: true,
        ])

    state = State.new() |> State.schedule_leader_election(self())

    {:ok, state}
  end

  def handle_info({:broadcast, message}, state) do
    data = RPC.encode(message)
    Pontoon.Membership.broadcast(data)

    {:noreply, state}
  end

  def handle_info({:udp, _socket, _ip, _port, data}, state) do
    case RPC.decode!(data) do
      {_msg, nil} ->
        Logger.info(">> unknown member, skipping this message")
        {:noreply, state}

      {msg, member} ->
        Logger.info(">> #{inspect member} said: #{inspect msg}")
        {reply, state} = State.handle_rpc(state, member, msg)

        if reply do
          # FIXME: not a broadcast
          send self(), {:broadcast, reply}
        end

        {:noreply, state}
    end
  end

  # TODO: Vote for self, Publish a RequestVote RPC
  def handle_info(:leader_election, state) do
    State.schedule_leader_election(state, self())

    state =
      case state.role do
        # Nothing to do if we're already the leader
        :leader ->
          Logger.info("Already leader... staying in power")
          state

        _ ->
          members = Pontoon.Membership.list()

          state = %{state |
                    role: :candidate,
                    voted_for: Pontoon.Membership.get_own_name(),
                    term: state.term + 1,
                    leader: %State.Leader{
                      votes: MapSet.new,
                      match_idx: members |> Enum.map(&{&1, 0}) |> Enum.into(%{}),
                      next_idx: members |> Enum.map(&{&1, 1}) |> Enum.into(%{})
                    },
                   }

          vote_message = %RPC.RequestVote{
            term: state.term,
            candidate: Pontoon.Membership.get_own_name(),
            last_log_idx: length(state.log), # TODO: fix me
            last_log_term: length(state.log)
          }

          Logger.warn("Initiating leadership election, voting for self #{inspect vote_message}...")

          send self(), {:broadcast, vote_message}

          state
    end

    {:noreply, state}
  end
end
