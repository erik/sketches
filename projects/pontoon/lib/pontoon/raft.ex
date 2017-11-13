defmodule Pontoon.Raft do
  require Logger
  use GenServer

  @rpc_port (System.get_env("RPC_PORT") || "9213") |> String.to_integer

  # Structs defining the format of RPC messages.
  defmodule RPC do
    @derive [Poison.Encoder]
    defstruct [:type, :data]

    def decode!(raw) do
      rpc = Poison.decode!(raw, as: %RPC{})
      case rpc.type do
        "AppendEntries" -> struct(AppendEntries, rpc.data)
        "RequestVote"   -> struct(RequestVote, rpc.data)
        unknown         ->
          Logger.error("unknown rpc message type: #{inspect unknown}")
          nil
      end
    end

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

    def schedule_leader_election(state, pid) do
      timer = Process.send_after(pid, :leader_election, @leader_election_interval_ms)

      # Clear any existing timer
      if state.election_timer do
        Process.cancel_timer(state.election_timer)
      end

      %{state | election_timer: timer}
    end

    # AppendEntries RPC implementation.
    def handle_rpc(state, %RPC.AppendEntries{} = rpc_message) do
      # Need to do some checks to see if we should accept this message
      accepted =
        cond do
        rpc_message.term < state.term ->
          Logger.warn("received message has older term: #{inspect rpc_message} ours: #{inspect state}")

      end

      state
    end

    def handle_rpc(state, %RPC.RequestVote{} = rpc_message) do
      Logger.warn("Not implemented: requestVote")
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

  def handle_info({:broadcast, message}, state) do

    {:noreply, state}
  end

  def handle_info({:udp, _socket, _ip, _port, _data}, state) do
    {:noreply, state}
  end

  # TODO: Vote for self, Publish a RequestVote RPC
  def handle_info(:leader_election, state) do
    Logger.warn("not yet implemented: leader election")

    {:noreply, state}
  end
end
