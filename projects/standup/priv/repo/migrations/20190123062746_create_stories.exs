defmodule Standup.Repo.Migrations.CreateStories do
  use Ecto.Migration

  def change do
    create table(:stories) do
      add :title, :string
      add :description, :text
      add :slug, :string
      add :tag_list, {:array, :string}
      add :author, references(:users, on_delete: :nothing)

      timestamps()
    end

    create index(:stories, [:author])
  end
end
