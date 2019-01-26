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

    resources "/users", UserController
    resources "/journals", JournalsController do
      resources "/entries", EntryController do
        resources "/replies", ReplyController
      end
    end
  end

  # Other scopes may use custom stacks.
  # scope "/api", StandupWeb do
  #   pipe_through :api
  # end
end
