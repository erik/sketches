defmodule Layabout.Slack do
  use Slack

  def get_active_users do
    Slack.Web.Users.list(%{presence: 1})
    |> Map.get("members")
    |> Enum.filter(fn(m) -> m["presence"] === "active" end)
  end

  def handle_connect(slack, state) do
    IO.puts "Connected as #{slack.me.name} (#{inspect slack.me})"
    {:ok, state}
  end

  def handle_event(message = %{type: "presence_change"}, slack, state) do
    user = Slack.Lookups.lookup_user_name(message.user, slack)
    is_online = case message.presence do
                  "away" -> false
                  "active" -> true
                end

    # IO.puts "#{user} is #{message.presence}"

    cond do
      is_online -> Layabout.Store.log_active(message.user)
      !is_online -> Layabout.Store.log_inactive(message.user)
    end

    {:ok, state}
  end

  # Just ignore everything other than presence changes
  def handle_event(_, _, state), do: {:ok, state}
  def handle_info(_, _, state), do: {:ok, state}
end
