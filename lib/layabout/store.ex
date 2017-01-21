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
    alias Timex.Duration

    get_user_record(user).entries
    |> Enum.reject(fn {_, e} -> is_nil(e) end)
    |> Enum.map(fn {b, e} ->
      minutes = Timex.diff(e, b, :minutes)

      Enum.into(0..minutes, [], fn(min) ->
        time = Timex.add(b, Duration.from_minutes(min))
        (60 * time.hour) + time.minute
      end)
    end)
    |> List.flatten
    |> Enum.group_by(&(&1))
    |> Map.to_list
    |> Enum.map(fn {time, vals} -> {time, length vals} end)
    |> Enum.into(%{})
  end

  def log_active(user) do
    now = DateTime.utc_now
    record = get_user_record(user)

    unless record[:meta] do
      # TODO: Lookup meta data
      # TODO: Need next point release of elixir-slack. 0.9.2 is broken on 1.4.0
    end

    entries =
      case record.entries do
        # First session seen.
        [] ->
          [{now, nil}]

        # Already have an open session.
        [{_, e} | _] when is_nil e ->
          record.entries

        [{b, e} | rest] ->
        # Being inactive < 5 minutes just merges into one record
        if Timex.diff(now, e, :minutes) < 5 do
          [{b, nil} | rest]
        else
          [{now, nil}, {b, e} | rest]
        end
      end

    Agent.update(__MODULE__, &Map.put(&1, user,
          Map.put(record, :entries, entries)))
  end

  def log_inactive(user) do
    now = DateTime.utc_now
    record = get_user_record(user)

    entries = case record.entries do
                [] -> []
                [{b, _} | rest] -> [{b, now} | rest]
              end

    Agent.update(__MODULE__, &Map.put(&1, user,
          Map.put(record, :entries, entries)))
  end

  defp get_user_record(user) do
    default = %{entries: [], meta: nil}
    Agent.get(__MODULE__, &Map.get(&1, user)) || default
  end
end
