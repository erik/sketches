defmodule Standup.Content.Entry do
  use Ecto.Schema
  import Ecto.Changeset


  schema "entries" do
    field :body, :string
    field :public, :boolean, default: false
    field :title, :string
    field :author_id, :id
    field :journal_id, :id

    timestamps()
  end

  @doc false
  def changeset(entry, attrs) do
    entry
    |> cast(attrs, [:title, :body, :public])
    |> validate_required([:title, :body, :public])
  end
end
