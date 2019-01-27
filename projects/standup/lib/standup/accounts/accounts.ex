defmodule Standup.Accounts do

  import Ecto.Query, warn: false
  alias Standup.Repo

  alias Standup.Accounts.User
  alias Standup.Accounts.Author

  #
  # Users
  #

  def get_or_create_user_by_email(email) do
    case Repo.get_by(User, email: email) do
      nil ->
        create_user(%{email: email})

      user ->
        {:ok, user}
    end
  end

  def list_users do
    Repo.all(User)
  end

  def get_user!(id), do: Repo.get!(User, id)

  def create_user(attrs \\ %{}) do
    %User{}
    |> User.changeset(attrs)
    |> Ecto.Changeset.cast_assoc(:author, with: &Author.changeset/2)
    |> Repo.insert()
  end

  def update_user(%User{} = user, attrs) do
    user
    |> User.changeset(attrs)
    |> Ecto.Changeset.cast_assoc(:author, with: &Author.changeset/2)
    |> Repo.update()
  end

  def delete_user(%User{} = user) do
    Repo.delete(user)
  end

  def change_user(%User{} = user) do
    User.changeset(user, %{})
  end

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
