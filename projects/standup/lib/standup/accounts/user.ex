defmodule Standup.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  alias Standup.Accounts.Author

  @required ~w(email)a
  @optional ~w(email_verified_at)a

  schema "users" do
    field :email, :string
    field :email_verified_at, :utc_datetime

    has_one :author, Author

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, @required ++ @optional)
    |> validate_required(@required)
    |> unique_constraint(:email)
  end
end
