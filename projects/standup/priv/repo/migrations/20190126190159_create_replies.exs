defmodule Standup.Repo.Migrations.CreateReplies do
  use Ecto.Migration

  def change do
    create table(:replies) do
      add :body, :text
      add :user_id, references(:users, on_delete: :nothing)
      add :entry_id, references(:entries, on_delete: :nothing)
      add :parent_id, references(:replies, on_delete: :nothing)

      timestamps()
    end

    create index(:replies, [:user_id])
    create index(:replies, [:entry_id])
    create index(:replies, [:parent_id])
  end
end
