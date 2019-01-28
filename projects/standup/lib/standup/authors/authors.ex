defmodule Standup.Authors do
  import Ecto.Query, warn: false
  alias Standup.Repo

  alias Standup.Authors.Author

  #
  # Authors
  #

  def list_authors do
    Repo.all(Author)
  end

  def get_author!(id), do: Repo.get!(Author, id)

  def create_author(attrs \\ %{}) do
    %Author{}
    |> Author.changeset(attrs)
    |> Repo.insert()
  end

  def update_author(%Author{} = author, attrs) do
    author
    |> Author.changeset(attrs)
    |> Repo.update()
  end

  def delete_author(%Author{} = author) do
    Repo.delete(author)
  end

  def change_author(%Author{} = author) do
    Author.changeset(author, %{})
  end
end
