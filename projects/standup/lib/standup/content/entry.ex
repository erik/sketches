defmodule Standup.Content.Entry do
  use Ecto.Schema
  import Ecto.Changeset

  alias Standup.Accounts.Author
  alias Standup.Content.Journal

  @required ~w(title body)a
  @optional ~w()a

  schema "entries" do
    field :title, :string
    field :body, :string

    belongs_to :author, Author
    belongs_to :journal, Journal

    timestamps()
  end

  @doc false
  def changeset(entry, attrs) do
    entry
    |> cast(attrs, @required ++ @optional)
    |> validate_required(@required)
  end
end
