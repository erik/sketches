defmodule Standup.Repo.Migrations.CreateUpdates do
  use Ecto.Migration

  def change do
    create table(:updates) do
      add :title, :string
      add :body, :text
      add :author, references(:users, on_delete: :nothing)
      add :story, references(:stories, on_delete: :nothing)

      timestamps()
    end

    create index(:updates, [:author])
    create index(:updates, [:story])
  end
end
