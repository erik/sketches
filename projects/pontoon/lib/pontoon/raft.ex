defmodule Pontoon.Raft do
  require Logger
  use GenServer

  @rpc_port (System.get_env("RPC_PORT") || "9213") |> String.to_integer

  # Structs defining the format of RPC messages.
  defmodule RPC do
    defmodule RequestVote do
      @derive [Poison.Encoder]
      defstruct [:term, :candidate, :last_log_idx, :last_log_term]
    end

    defmodule AppendEntries do
      @derive [Poison.Encoder]
      defstruct [:term, :leader, :prev_log_idx, :prev_log_term, :leader_commit, :entries]
    end
  end

  defmodule State do
    @leader_election_interval_ms 400

    defstruct [:leader, :term, :log, :commit_log, :commit_idx, :election_timer]

    def new() do
      %__MODULE__{leader: nil, term: 0, log: [], commit_log: [], commit_idx: 0}
    end

    # TODO: a cancelable timer for when to elect self as leader
    # TODO: maybe move this to Raft module
    def schedule_leader_election(state, pid) do
      timer = Process.send_after(pid, :leader_election, @leader_election_interval_ms)

      if state.election_timer do
        Process.cancel_timer(state.election_timer)
      end

      %{state | election_timer: timer}
    end

    def append_entries(state, entries) do
      state
    end
  end

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def init(:ok) do
    {:ok, _socket} = :gen_udp.open(@rpc_port, [
          :binary,
          :inet,
          {:active, true}
        ])

    state = State.new() |> State.schedule_leader_election(self())

    {:ok, state}
  end

  def handle_info({:udp, _socket, _ip, _port, _data}, state) do
    {:noreply, state}
  end

  def handle_info(:leader_election, state) do
    Logger.warn("not yet implemented: leader election")
    {:noreply, state}
  end
end
