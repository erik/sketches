defmodule Standup.Post.Story do
  use Ecto.Schema
  import Ecto.Changeset

  alias Standup.Accounts.User
  alias Standup.Post.Update

  @required_fields ~w(title description user_id)a
  @optional_fields ~w(slug tag_list)a

  schema "stories" do
    field :title, :string
    field :description, :string
    field :slug, :string
    field :tag_list, {:array, :string}

    belongs_to(:author, User, foreign_key: :user_id)
    has_many(:updates, Update)

    timestamps()
  end

  @doc false
  def changeset(story, attrs) do
    story
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
    |> slugify_title()
  end

  defp slugify_title(changeset) do
    if title = get_change(changeset, :title) do
      put_change(changeset, :slug, slugify(title))
    else
      changeset
    end
  end

  defp slugify(text) do
    text
    |> String.downcase()
    |> String.replace(~r/[^\w-]+/u, "-")
  end
end
