defmodule StandupWeb.Router do
  use StandupWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers

    plug Guardian.Plug.Pipeline,
      module: StandupWeb.Guardian,
      error_handler: StandupWeb.AuthController

    plug Guardian.Plug.VerifySession
    plug Guardian.Plug.LoadResource, allow_blank: true
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", StandupWeb do
    pipe_through :browser

    get "/", PageController, :index

    get "/login", AuthController, :new
    post "/login", AuthController, :create
    get "/login/:magic_token", AuthController, :callback
    get "/logout", AuthController, :destroy

    resources "/users", UserController
    resources "/authors", AuthorController

    resources "/journals", JournalController do
      resources "/entries", EntryController do
        resources "/replies", ReplyController
      end
    end
  end
end
