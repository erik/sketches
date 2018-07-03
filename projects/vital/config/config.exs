# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :vital,
  ecto_repos: [Vital.Repo]

# Configures the endpoint
config :vital, VitalWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "yqhB9K1RuOeJQlckC9+Re4HftcFEz+qfFubLqC5lipm5YJBpjSSkx7EluQecL03B",
  render_errors: [view: VitalWeb.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Vital.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:user_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
