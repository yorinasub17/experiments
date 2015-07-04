defmodule Capture.PageController do
  use Capture.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
