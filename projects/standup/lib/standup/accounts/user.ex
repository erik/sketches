defmodule Standup.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  @required_fields ~w(email name)a
  @optional_fields ~w(bio location image_url)a

  schema "users" do
    field :email, :string, unique: true

    field :name, :string
    field :bio, :string
    field :location, :string
    field :image_url, :string

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> validate_format(:email, ~r/@/)
    |> unique_constraint(:email)
  end
end
