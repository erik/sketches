defmodule Layabout do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    IO.puts IO.ANSI.green <> "Start application" <> IO.ANSI.reset

    slack_token = Application.get_env(:slack, :api_token)
    unless slack_token, do: exit "need to set SLACK_API_TOKEN"

    children = [
      worker(Slack.Bot, [Layabout.Slack, [], slack_token]),
      worker(Layabout.Store, [])
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
