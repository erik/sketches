# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.

# General application configuration
use Mix.Config

config :standup,
  ecto_repos: [Standup.Repo]

# Configures the endpoint
config :standup, StandupWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "QvMzpHh48GqtNIgqEMQhwP2d6Wz6IhP2R9nJTrOXBPiadXpOaxKVMMWm0hEF8E1g",
  render_errors: [view: StandupWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Standup.PubSub, adapter: Phoenix.PubSub.PG2]

config :standup, Standup.Guardian,
  issuer: "standup",
  secret_key: "a super secret value"

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Use Jason for JSON parsing in Phoenix
config :phoenix, :json_library, Jason

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env()}.exs"
