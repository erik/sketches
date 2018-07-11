defmodule VitalsignsWeb.Router do
  use VitalsignsWeb, :router

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

  scope "/", VitalsignsWeb do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index

    resources "/users", UserController
    resources "/posts", PostController
    resources "/attachments", AttachmentController
  end

   scope "/api", VitalsignsWeb do
    pipe_through :api
   end
end
