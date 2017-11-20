defmodule Pontoon.Raft do
  require Logger
  use GenServer

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
    @leader_election_interval_ms 1000
    @append_entries_interval_ms 500

    defstruct role: :follower, leader: nil, term: 0, log: [], commit_log: [],
      commit_idx: 0, election_timer: nil, leader_state: nil, voted_for: nil

    defmodule Leader do
      defstruct [:votes, :match_idx, :next_idx]
    end

    def reset_election_timer(state, pid) do
      timer = Process.send_after(pid, :leader_election, @leader_election_interval_ms)

      # Clear any existing timer
      if state.election_timer do
        Process.cancel_timer(state.election_timer)
      end

      %{state | election_timer: timer}
    end

    def schedule_append_entries(pid) do
      Process.send_after(pid, {:append_entries, []}, @append_entries_interval_ms)
    end

    def get_last_log_term(state) do
      case List.last(state.log) do
        nil -> 0
        tl -> tl.term
      end
    end

    def maybe_step_down(state, member, remote_term) do
      if state.term < remote_term do
        Logger.info("#{member.name} has higher term! Stepping down")
        %{state | term: remote_term, role: :follower, voted_for: nil, leader: member.name}
      else
        state
      end
    end

    # AppendEntries RPC implementation.
    def handle_rpc(state, member, %RPC.AppendEntries{} = rpc) do
      state = maybe_step_down(state, member, rpc.term)
      |> State.reset_election_timer(self())

      {accept_append, state} =
        cond do
          rpc.term < state.term ->
            {false, state}

          rpc.prev_log_idx >= length(state.log) ->
            {false, state}

          state.log[rpc.prev_log_idx].term != rpc.prev_log_term ->
            # Follow the leader, delete mismatched log items.
            # FIXME: should handle earlier inconsistencies as well.
            log = state.log |> Enum.take(rpc.prev_log_idx - 1)

            {false, %{state | log: log}}

          # Success! append entries
          true ->
            log = state.log ++ rpc.entries
            commit =
              if rpc.leader_commit > state.commit_idx do
                # FIXME: unsure if length(log) is correct
                min(rpc.leader_commit, length(log) - 1)
              else
                state.commit_idx
              end

            {true, %{state | log: log, commit_idx: commit}}
        end

      reply = %RPC.ReplyAppendEntries{term: state.term, success: accept_append}
      {reply, state}
    end

    # RequestVote RPC implementation
    def handle_rpc(state, member, %RPC.RequestVote{} = rpc) do
      state = maybe_step_down(state, member, rpc.term)
      |> State.reset_election_timer(self())

      vote_granted =
        cond do
          rpc.term < state.term ->
            Logger.info("rejecting #{member.name} for lower term #{rpc.term} vs #{state.term}")
            false

          state.voted_for && state.voted_for != rpc.candidate ->
            Logger.info("rejecting #{member.name} because already voted")
            false

          # FIXME: idk if commit_idx is correct
          rpc.last_log_idx < state.commit_idx ->
            Logger.info("rejecting #{member.name} for last_log_idx")
            false

          # Everything checks out!
          true -> true
        end

      state =
        if vote_granted do
          %{state | role: :follower, voted_for: rpc.candidate}
        else
          state
        end

      reply = %RPC.ReplyRequestVote{term: state.term, granted: vote_granted}
      {reply, state}
    end

    def handle_rpc(state, member, %RPC.ReplyRequestVote{} = rpc) do
      Logger.info(">> #{member.name} voted #{rpc.granted}")

      state =
        case state.role do
          # Vote only matters if we're still a candidate
          :candidate ->
            votes =
              if rpc.granted do
                MapSet.put(state.leader_state.votes, member.name)
              else
                state.leader_state.votes
              end

            vote_count = MapSet.size(votes)
            majority_votes = div(Map.size(Pontoon.Membership.list()), 2) + 1

            {role, term} =
                if vote_count >= majority_votes do
                  Logger.warn("Received majority #{vote_count}/#{majority_votes}! Seizing power")
                  {:leader, state.term + 1}
                else
                  Logger.info("Not enough votes. Election not over yet, #{vote_count}/#{majority_votes}")
                  {:candidate, state.term}
                end

            %{state |
              role: role,
              term: term,
              leader_state: %{state.leader_state | votes: votes}}

          _else ->
            state
        end

      {nil, state}
    end

    def handle_rpc(state, _member, %RPC.ReplyAppendEntries{} = _rpc) do
      # Logger.info("Got append entries reply: #{inspect rpc}")
      state = State.reset_election_timer(state, self())

      # TODO: write me

      {nil, state}
    end
  end

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(opts) do
    Logger.info("Starting Raft on port #{inspect opts[:rpc_port]}")
    {:ok, _socket} = :gen_udp.open(opts[:rpc_port], [
          :binary,
          ip: {0, 0, 0, 0},
          active: true,
          reuseaddr: true,
        ])

    state = %State{} |> State.reset_election_timer(self())
    State.schedule_append_entries(self())

    {:ok, state}
  end

  def send_broadcast(message) do
    RPC.encode(message) |> Pontoon.Membership.send_broadcast
  end

  def send_to(%Pontoon.Member{} = member, message) do
    encoded = RPC.encode(message)
    Pontoon.Membership.send_to(member, encoded)
  end

  def handle_info({:udp, _socket, _ip, _port, data}, state) do
    case RPC.decode!(data) do
      {_msg, nil} ->
        Logger.info(">> unknown member, skipping this message")
        {:noreply, state}

      {msg, member} ->
        {reply, state} = State.handle_rpc(state, member, msg)

        if reply, do: send_to(member, reply)

        {:noreply, state}
    end
  end

  def handle_info({:append_entries, entries}, state) do
    # FIXME: no reason this is attached to State
    State.schedule_append_entries(self())

    case state.role do
      :leader ->
        send_broadcast(%RPC.AppendEntries{
              term: state.term,
              leader: Pontoon.Membership.get_own_name(),
              prev_log_idx: state.commit_idx,
              prev_log_term: State.get_last_log_term(state),
              leader_commit: nil,
              entries: entries,
        })

        {:noreply, state}

      _else ->
        {:noreply, state}
    end
  end

  def handle_info(:leader_election, state) do
    state = State.reset_election_timer(state, self())

    state =
      case state.role do
        # Nothing to do if we're already the leader
        :leader ->
          Logger.info("Already leader... staying in power")
          state

        # If we became a candidate in the previous cycle, use this
        # cycle to count votes and declare election results
        :candidate ->
          votes = state.leader_state.votes |> MapSet.size
          majority_votes = div(Map.size(Pontoon.Membership.list()), 2) + 1

          Logger.info("Lost election (timeout)... #{votes} votes. (needed: #{majority_votes})")
          %{state | voted_for: nil, role: :follower}

        # Initiate possible regime change.
        :follower ->
          members = Pontoon.Membership.list()

          state = %{state |
                    role: :candidate,
                    voted_for: Pontoon.Membership.get_own_name(),
                    term: state.term + 1,
                    leader_state: %State.Leader{
                      votes: MapSet.new,
                      match_idx: members |> Enum.map(&{&1, 0}) |> Enum.into(%{}),
                      next_idx: members |> Enum.map(&{&1, 1}) |> Enum.into(%{})
                    },
                   }

          vote_message = %RPC.RequestVote{
            term: state.term,
            candidate: Pontoon.Membership.get_own_name(),
            last_log_idx: length(state.log),
            last_log_term: State.get_last_log_term(state)
          }

          Logger.warn("No leader detected! Starting election.")

          send_broadcast(vote_message)

          state
    end

    {:noreply, state}
  end
end
