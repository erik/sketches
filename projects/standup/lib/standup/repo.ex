defmodule Standup.Repo do
  use Ecto.Repo,
    otp_app: :standup,
    adapter: Ecto.Adapters.Postgres
end
