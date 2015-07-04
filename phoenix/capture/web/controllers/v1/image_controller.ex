defmodule Capture.Api.V1.ImageController do
  use Capture.Web, :controller

  alias Capture.Image

  def index(conn, _params) do
    images = Repo.all(Image)
    render(conn, "list.json", data: images)
  end

  def create(conn, params) do
    changeset = Image.changeset(%Image{}, params)

    if changeset.valid? do
      image = Repo.insert!(changeset)
      render(conn, "show.json", data: image)
    else
      # TODO: Show error message
    end
  end

  def show(conn, %{"id" => id}) do
    image = Repo.get!(Image, id)
    render(conn, "show.json", data: image)
  end

  def update(conn, params) do
    image = Repo.get!(Image, params["id"])
    changeset = Image.changeset(image, params)

    if changeset.valid? do
      Repo.update!(changeset)
      image = Repo.get!(Image, params["id"])
      render(conn, "show.json", data: image)
    else
      # TODO: Show error message
    end
  end

  def delete(conn, %{"id" => id}) do
    image = Repo.get!(Image, id)
    Repo.delete!(image)
    render(conn, "delete.json", data: id)
  end
end
