defmodule Standup.Repo.Migrations.CreateJournals do
  use Ecto.Migration

  def change do
    create table(:journals) do
      add :title, :text
      add :description, :text
      add :tags, {:array, :text}
      add :public, :boolean, default: false, null: false
      add :started_at, :utc_datetime
      add :completed_at, :utc_datetime
      add :author_id, references(:authors, on_delete: :nothing)

      timestamps()
    end

    create index(:journals, [:author_id])
  end
end
