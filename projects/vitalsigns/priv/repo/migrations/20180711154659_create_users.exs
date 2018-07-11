defmodule Vitalsigns.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :name, :string
      add :email, :string
      add :pw_hash, :string

      timestamps()
    end

  end
end
