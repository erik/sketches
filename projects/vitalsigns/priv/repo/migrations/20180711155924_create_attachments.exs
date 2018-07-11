defmodule Vitalsigns.Repo.Migrations.CreateAttachments do
  use Ecto.Migration

  def change do
    create table(:attachments) do
      add :caption, :text
      add :kind, :string
      add :url, :text
      add :body, :text
      add :post_id, references(:posts, on_delete: :nothing)

      timestamps()
    end

    create index(:attachments, [:post_id])
  end
end
