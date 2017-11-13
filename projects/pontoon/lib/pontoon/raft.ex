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

    def decode!(raw) do
      rpc = Poison.decode!(raw, as: %RPC{})
      member = Pontoon.Membership.get_member(rpc.name)

      msg =
        case rpc.type do
          "AppendEntries" -> struct(AppendEntries, rpc.data)
          "RequestVote"   -> struct(RequestVote, rpc.data)
          unknown         ->
            Logger.error("unknown RPC message type: #{inspect unknown}")
            nil
        end

      {msg, member}
    end

    def encode(%AppendEntries{} = message) do
      encode("AppendEntries", Poison.encode!(message))
    end

    def encode(%RequestVote{} = message) do
      encode("RequestVotes", Poison.encode!(message))
    end

    def encode(type, data) do
      name = Pontoon.Membership.get_own_name()

      Poison.encode!(%RPC{type: type, data: data, name: name})
    end
  end

  defmodule State do
    @leader_election_interval_ms 400

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

    # AppendEntries RPC implementation.
    def handle_rpc(state, _member, %RPC.AppendEntries{} = rpc_message) do
      # Need to do some checks to see if we should accept this message
      accepted =
        cond do
        rpc_message.term < state.term ->
          Logger.warn("received message has older term: #{inspect rpc_message} \
ours: #{inspect state}")

      end

      state
    end

    # RequestVote RPC implementation
    def handle_rpc(state, _member, %RPC.RequestVote{} = rpc_message) do
      Logger.warn("Not implemented: requestVote")
      state
    end
  end

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def init(:ok) do
    port = (System.get_env("RPC_PORT") || @rpc_port)
    |> String.to_integer

    Logger.info("starting with port #{inspect port}")
    {:ok, _socket} = :gen_udp.open(port, [
          :binary,
          :inet,
          {:active, true}
        ])

    state = State.new() |> State.schedule_leader_election(self())

    {:ok, state}
  end

  def handle_info({:broadcast, message}, state) do
    data = Poison.encode!(message)
    Pontoon.Membership.broadcast(data)

    {:noreply, state}
  end

  def handle_info({:udp, _socket, _ip, _port, data}, state) do
    {msg, member} = RPC.decode!(data)
    {reply, state} = State.handle_rpc(state, member, msg)

    send self(), {:broadcast, reply}

    {:noreply, state}
  end

  # TODO: Vote for self, Publish a RequestVote RPC
  def handle_info(:leader_election, state) do

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

    {:noreply, state}
  end
end
