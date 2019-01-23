defmodule Standup.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :email, :string
      add :name, :string
      add :bio, :text
      add :location, :string
      add :image_url, :string

      timestamps()
    end

    create index("users", [:email], unique: true)
  end
end
