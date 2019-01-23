defmodule Standup.Repo.Migrations.CreateUpdates do
  use Ecto.Migration

  def change do
    create table(:updates) do
      add :title, :string
      add :body, :text

      add :user_id, references(:users, on_delete: :nothing)
      add :story_id, references(:stories, on_delete: :nothing)

      timestamps()
    end

    create index(:updates, [:user_id])
    create index(:updates, [:story_id])
  end
end
