# FIXME: Maybe this is more accurately a node?
defmodule Pontoon.Member do
  defstruct [:address, :port, :last_seen]
end

defmodule Pontoon.Membership do
  require Logger
  use Agent

  @max_member_age_ms 20 * 60 * 1000
  @prune_interval_ms 10 * 1000


  def start_link(_opts \\ []) do
    Logger.info("initializing membership list")

    {:ok, pid} = Agent.start_link(fn -> %{} end, name: __MODULE__)

    Task.start_link(fn ->
      schedule_prune_members()
    end)

    {:ok, pid}
  end

  def add_member(key, member) do
    Agent.update(__MODULE__, &Map.put(&1, key, member))
  end

  def remove_member(key) do
    Agent.update(__MODULE__, &Map.delete(&1, key))
  end

  def prune_members() do
    now = DateTime.utc_now()

    Agent.update(__MODULE__, fn members ->
      members
      |> Enum.filter(fn {k, v} ->
        diff = DateTime.diff(now, v.last_seen, :milliseconds)
        should_keep = diff < @max_member_age_ms

        if not should_keep do
          Logger.warn("#{k} has timed out... removing!")
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
