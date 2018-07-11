defmodule Vitalsigns.Web.Post do
  use Ecto.Schema
  import Ecto.Changeset


  schema "posts" do
    field :body, :string
    field :location, :string
    field :title, :string
    field :user, :id

    timestamps()
  end

  @doc false
  def changeset(post, attrs) do
    post
    |> cast(attrs, [:title, :body, :location])
    |> validate_required([:title, :body, :location])
  end
end
