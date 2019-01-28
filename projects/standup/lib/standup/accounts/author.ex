defmodule Standup.Accounts.Author do
  use Ecto.Schema
  import Ecto.Changeset

  alias Standup.Accounts.User
  alias Standup.Content.Journal

  @required ~w(name user_id)a
  @optional ~w(bio image_url location)a

  schema "authors" do
    field :name, :string

    field :bio, :string
    field :image_url, :string
    field :location, :string

    belongs_to :user, User
    has_many :journals, Journal

    timestamps()
  end

  @doc false
  def changeset(author, attrs) do
    author
    |> cast(attrs, @required ++ @optional)
    |> validate_required(@required)
  end
end
