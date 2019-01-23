defmodule StandupWeb.Router do
  use StandupWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", StandupWeb do
    pipe_through :browser

    get "/", PageController, :index

    resources "/user", UserController
    resources "/story", StoryController
    resources "/update", UpdateController
  end

  scope "/api", StandupWeb do
    pipe_through :api

    # TODO: define API interface??
  end
end
