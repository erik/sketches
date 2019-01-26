defmodule Standup.Repo.Migrations.CreateAuthors do
  use Ecto.Migration

  def change do
    create table(:authors) do
      add :name, :text
      add :image_url, :text
      add :bio, :text
      add :location, :text
      add :user_id, references(:users, on_delete: :nothing)

      timestamps()
    end

    create index(:authors, [:user_id])
  end
end
