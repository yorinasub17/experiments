defmodule Capture.Router do
  use Capture.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", Capture do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    resources "/images", ImageController
  end

  scope "/api", Capture.Api, as: :api do
    pipe_through :api

    scope "/v1", V1, as: :v1 do
      resources "/images", ImageController, except: [:edit, :new]
    end
  end
end
