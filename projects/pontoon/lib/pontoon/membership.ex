# FIXME: Maybe this is more accurately a node?
defmodule Pontoon.Member do
  defstruct [:address, :port, :last_seen, :name]
end

defmodule Pontoon.Membership do
  require Logger
  use Agent

  @max_member_age_ms 20 * 60 * 1000
  @prune_interval_ms 10 * 1000


  def start_link(_opts \\ []) do
    Logger.info("Initializing membership list")

    {:ok, pid} = Agent.start_link(fn -> %{} end, name: __MODULE__)

    Task.start_link(fn ->
      schedule_prune_members()
    end)

    {:ok, pid}
  end

  def get_own_name() do
    Kernel.node() |> Atom.to_string()
  end

  def list() do
    Agent.get(__MODULE__, &(&1))
  end

  def send_broadcast(data) do
    Pontoon.Membership.list()
    |> Enum.map(fn {_k, v} -> send_to(v, data) end)
  end

  def send_to(%Pontoon.Member{address: addr, port: port}, data) when is_binary(data) do
    {:ok, sock} = :gen_udp.open(0, [:binary])
    :ok = :gen_udp.send(sock, addr, port, data)
    :ok = :gen_udp.close(sock)
  end

  def send_to(name, data) when is_binary(name) do
    member = get_member(name)
    send_to(member, data)
  end

  def get_member(key) do
    Agent.get(__MODULE__, &Map.get(&1, key))
  end

  def add_member(key, member) do
    Agent.update(__MODULE__, &Map.put(&1, key, member))
  end

  def remove_member(key) do
    Logger.info("QUIT: #{key}")
    Agent.update(__MODULE__, &Map.delete(&1, key))
  end

  # This goes against the raft spec... We technically shouldn't remove
  # nodes so easily, but since this is a educational exercise, I'm
  # going to go the more convenient route.
  def prune_members() do
    now = DateTime.utc_now()

    Agent.update(__MODULE__, fn members ->
      members
      |> Enum.filter(fn {k, v} ->
        diff = DateTime.diff(now, v.last_seen, :milliseconds)
        should_keep = diff < @max_member_age_ms

        if not should_keep do
          Logger.error("#{k} has timed out... removing!")
        end

        should_keep
      end)
      |> Enum.into(%{})
    end)
  end

  defp schedule_prune_members() do
    Process.sleep(@prune_interval_ms)

    :ok = prune_members()

    # There is no escape.
    schedule_prune_members()
  end
end
