defmodule Layabout.Slack do
  use Slack

  def get_active_users do
    Slack.Web.Users.list(%{presence: 1})
    |> Map.get("members")
    |> Enum.filter(fn(m) -> m["presence"] === "active" end)
  end

  def handle_connect(slack, state) do
    IO.puts "Connected as #{slack.me.name}"
    {:ok, state}
  end

  def handle_event(message = %{type: "presence_change"}, slack, state) do
    user = Slack.Lookups.lookup_user_name(message.user, slack)
    is_online = case message.presence do
                  "away" -> false
                  "active" -> true
                end
    IO.puts "presence change -> #{user}, online: #{is_online}"
    {:ok, state}
  end

  def handle_event(message, _, state) do
    IO.puts "unknown message type: #{message.type}: #{inspect message}"
    {:ok, state}
  end

  def handle_info(_, _, state), do: {:ok, state}
end
