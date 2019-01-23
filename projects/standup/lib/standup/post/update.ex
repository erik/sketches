defmodule Standup.Post.Update do
  use Ecto.Schema
  import Ecto.Changeset

  alias Standup.Accounts.User
  alias Standup.Post.Story

  @required_fields ~w(title body user_id story_id)a
  @optional_fields ~w()a

  schema "updates" do
    field :title, :string
    field :body, :string

    belongs_to(:author, User, foreign_key: :user_id)
    belongs_to(:story, Story, foreign_key: :story_id)

    timestamps()
  end

  @doc false
  def changeset(update, attrs) do
    update
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end
end
