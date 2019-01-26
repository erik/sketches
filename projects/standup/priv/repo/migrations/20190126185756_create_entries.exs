defmodule Standup.Repo.Migrations.CreateEntries do
  use Ecto.Migration

  def change do
    create table(:entries) do
      add :title, :text
      add :body, :text
      add :public, :boolean, default: false, null: false
      add :author_id, references(:authors, on_delete: :nothing)
      add :journal_id, references(:journals, on_delete: :nothing)

      timestamps()
    end

    create index(:entries, [:author_id])
    create index(:entries, [:journal_id])
  end
end
