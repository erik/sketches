defmodule Layabout.Store do
  def start_link do
    IO.puts "Starting store."
    {:ok, pid} = Agent.start_link(fn -> %{} end, name: __MODULE__)

    Layabout.Slack.get_active_users
    |> Enum.map(&Map.get(&1, "id"))
    |> Enum.map(&log_active(&1))

    {:ok, pid}
  end

  def get_user_record(user) do
    default = %{entries: [[nil, nil]], meta: nil}
    Agent.get(__MODULE__, &Map.get(&1, user)) || default
  end

  def log_active(user) do
    now = DateTime.utc_now
    record = get_user_record(user)

    unless record[:meta] do
      # TODO: lookup
    end

    [hd|rest] = record[:entries]
    next =
      case hd do
        [start, nil] -> [[start, nil]]
        x            -> [[now, nil], x]
      end

    Agent.update(__MODULE__, &Map.put(&1, user,
          Map.put(record, :entries, rest ++ next)))
  end

  def log_inactive(user) do
    now = DateTime.utc_now
    record = get_user_record(user)

    [[b, _]|rest] = record[:entries]

    Agent.update(__MODULE__, &Map.put(&1, user,
          Map.put(record, :entries, [[b, now]] ++ rest)))
  end

end
