defmodule Standup.Social.Reply do
  use Ecto.Schema
  import Ecto.Changeset


  schema "replies" do
    field :body, :string
    field :user_id, :id
    field :entry_id, :id
    field :parent_id, :id

    timestamps()
  end

  @doc false
  def changeset(reply, attrs) do
    reply
    |> cast(attrs, [:body])
    |> validate_required([:body])
  end
end
