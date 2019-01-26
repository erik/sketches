defmodule Standup.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset


  schema "users" do
    field :email, :string
    field :email_verified_at, :utc_datetime

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :email_verified_at])
    |> validate_required([:email, :email_verified_at])
  end
end
