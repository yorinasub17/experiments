defmodule Capture.Api.V1.ImageView do
  use Capture.Web, :view
  @attributes [:id, :title, :uuid]

  def render("list.json", %{data: images}) do
    for image <- images, do: render("show.json", data: image)
  end

  def render("show.json", %{data: image}) do
    image
    |> Map.take(@attributes)
  end

  def render("delete.json", %{data: id}) do
    %{"id": id}
  end
end
