defmodule Standup.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :email, :string
      add :email_verified_at, :utc_datetime

      timestamps()
    end

  end
end
