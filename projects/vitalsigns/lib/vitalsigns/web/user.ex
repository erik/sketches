defmodule Vitalsigns.Web.User do
  use Ecto.Schema
  import Ecto.Changeset


  schema "users" do
    field :email, :string
    field :name, :string
    field :pw_hash, :string

    timestamps()
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:name, :email, :pw_hash])
    |> validate_required([:name, :email, :pw_hash])
  end
end
