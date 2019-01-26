defmodule Standup.Content.Author do
  use Ecto.Schema
  import Ecto.Changeset


  schema "authors" do
    field :bio, :string
    field :image_url, :string
    field :location, :string
    field :name, :string
    field :user_id, :id

    timestamps()
  end

  @doc false
  def changeset(author, attrs) do
    author
    |> cast(attrs, [:name, :image_url, :bio, :location])
    |> validate_required([:name, :image_url, :bio, :location])
  end
end
