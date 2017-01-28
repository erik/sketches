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

    get_user_record(user)
    |> Enum.map(fn {b, e} ->
      span_end = e || DateTime.utc_now
      duration = Timex.diff(span_end, b, :minutes)

      for min <- 0..duration do
        time = Timex.add(b, Duration.from_minutes(min))
        {time.hour, 5 * div(time.minute, 5)}
      end
    end)
    |> List.flatten
    |> Enum.group_by(&(&1))
    |> Enum.map(fn {{hr, min}, vals} ->
      bin = :io_lib.format("~2..0B:~2..0B", [hr, min])
      %{bin: List.to_string(bin), count: length vals}
    end)
  end

  def log_active(user, timestamp \\ DateTime.utc_now) do
    entries =
      case get_user_record(user) do
        # First session seen.
        [] ->
          [{timestamp, nil}]

        [{b, e} | rest] when not is_nil(e) ->
          # Being inactive < 5 minutes just merges into one record
          if Timex.diff(timestamp, e, :minutes) < 5 do
            [{b, nil} | rest]
          else
            [{timestamp, nil}, {b, e} | rest]
          end

        # Already have an open session.
        e -> e
      end

    Agent.update(__MODULE__, &Map.put(&1, user, entries))
  end

  def log_inactive(user, timestamp \\ DateTime.utc_now) do
    entries =
      case get_user_record(user) do
        [] ->
          []
        [{b, _} | rest] ->
          [{b, timestamp} | rest]
      end

    Agent.update(__MODULE__, &Map.put(&1, user, entries))

  end

  defp get_user_record(user) do
    Agent.get(__MODULE__, &Map.get(&1, user)) || []
  end
end
