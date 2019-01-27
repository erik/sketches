defmodule Standup.Repo.Migrations.Seed do
  use Ecto.Migration

  def change do

    # Users
    create table(:users) do
      add :email, :string
      add :email_verified_at, :utc_datetime

      timestamps()
    end

    create unique_index(:users, [:email])

    # Authors
    create table(:authors) do
      add :name, :text
      add :image_url, :text
      add :bio, :text
      add :location, :text

      add :user_id, references(:users, on_delete: :delete_all),
        null: false

      timestamps()
    end

    create unique_index(:authors, [:user_id])

    # Journals
    create table(:journals) do
      add :title, :text
      add :description, :text
      add :tags, {:array, :text}
      add :public, :boolean, default: true, null: false
      add :started_at, :utc_datetime
      add :completed_at, :utc_datetime

      add :author_id, references(:authors, on_delete: :nothing),
        null: false

      timestamps()
    end

    create index(:journals, [:author_id])

    # Entries
    create table(:entries) do
      add :title, :text
      add :body, :text

      add :author_id, references(:authors, on_delete: :nothing),
        null: false

      add :journal_id, references(:journals, on_delete: :nothing),
        null: false

      timestamps()
    end

    create index(:entries, [:author_id])
    create index(:entries, [:journal_id])

    # Replies
    create table(:replies) do
      add :body, :text

      add :user_id, references(:users, on_delete: :nothing),
        null: false
      add :entry_id, references(:entries, on_delete: :nothing),
        null: false
      add :parent_id, references(:replies, on_delete: :nothing)

      timestamps()
    end

    create index(:replies, [:user_id])
    create index(:replies, [:entry_id])
    create index(:replies, [:parent_id])
  end
end
