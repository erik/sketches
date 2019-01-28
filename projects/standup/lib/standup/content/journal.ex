defmodule Standup.Content.Journal do
  use Ecto.Schema
  import Ecto.Changeset

  alias Standup.Authors.Author
  alias Standup.Content.Entry

  @required ~w(title description public tags)a
  @optional ~w(started_at completed_at)a

  schema "journals" do
    field :title, :string
    field :description, :string

    field :public, :boolean, default: true
    field :tags, {:array, :string}, default: []

    field :started_at, :utc_datetime
    field :completed_at, :utc_datetime

    belongs_to :author, Author
    has_many :entries, Entry

    timestamps()
  end

  @doc false
  def changeset(journal, attrs) do
    journal
    |> cast(attrs, @required ++ @optional)
    |> validate_required(@required)
  end
end
