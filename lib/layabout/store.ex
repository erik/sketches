defmodule Layabout.Store do
  use Timex

  def start_link do
    IO.puts "Starting store."
    {:ok, pid} = Agent.start_link(fn -> %{} end, name: __MODULE__)

    Layabout.Slack.get_active_users
    |> Enum.map(&Map.get(&1, "id"))
    |> Enum.map(&log_active(&1))

    {:ok, pid}
  end

  def get_histogram(user) do
    hist = 0..23 |> Enum.map(&{&1, 0}) |> Enum.into(%{})

    {_, hist} = get_user_record(user)[:entries]
    |> Enum.reject(fn {_, e} -> e == nil end)
    |> Enum.map(fn {b, e} ->
      hours = Timex.diff(e, b, :hours)

      Enum.into(0..hours, [], fn hr ->
        Timex.add(b, Timex.Duration.from_hours(hr)).hour
      end)
    end)
    |> List.flatten
    |> Enum.map_reduce(hist, fn x, acc ->
      Map.get_and_update(acc, x, &{&1, &1 + 1})
    end)

    hist
  end

  def log_active(user) do
    now = DateTime.utc_now
    record = get_user_record(user)

    unless record[:meta] do
      # TODO: Lookup meta data
      # TODO: Need next point release of elixir-slack. 0.9.2 is broken on 1.4.0
    end

    [hd|rest] = record[:entries]
    next =
      case hd do
        {nil, nil} -> [{now, nil}]
        {b, nil}   -> [{b, nil}]
        {b, e} ->
          cond do
            # Being inactive < 5 minutes just merges into one record
            Timex.diff(now, e, :minutes) < 5  -> [{b, nil}]
            true -> [{now, nil}, {b, e}]
          end
      end

    Agent.update(__MODULE__, &Map.put(&1, user,
          Map.put(record, :entries, rest ++ next)))
  end

  def log_inactive(user) do
    now = DateTime.utc_now
    record = get_user_record(user)

    [{b, _}|rest] = record[:entries]

    Agent.update(__MODULE__, &Map.put(&1, user,
          Map.put(record, :entries, [{b, now}] ++ rest)))
  end

  defp get_user_record(user) do
    default = %{entries: [{nil, nil}], meta: nil}
    Agent.get(__MODULE__, &Map.get(&1, user)) || default
  end
end
