defmodule Standup.Content.Journal do
  use Ecto.Schema
  import Ecto.Changeset


  schema "journals" do
    field :completed_at, :utc_datetime
    field :description, :string
    field :public, :boolean, default: false
    field :started_at, :utc_datetime
    field :tags, {:array, :string}
    field :title, :string
    field :author_id, :id

    timestamps()
  end

  @doc false
  def changeset(journal, attrs) do
    journal
    |> cast(attrs, [:title, :description, :tags, :public, :started_at, :completed_at])
    |> validate_required([:title, :description, :tags, :public, :started_at, :completed_at])
  end
end
