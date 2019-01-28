defmodule Standup.Accounts do

  import Ecto.Query, warn: false
  alias Standup.Repo

  alias Standup.Accounts.User
  alias Standup.Authors.Author

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
end
