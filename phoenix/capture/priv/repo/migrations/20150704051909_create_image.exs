defmodule Capture.Repo.Migrations.CreateImage do
  use Ecto.Migration

  def change do
    create table(:images) do
      add :title, :string
      add :uuid, :string

      timestamps
    end

  end
end
