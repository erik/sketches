defmodule VitalWeb.Router do
  use VitalWeb, :router

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

  scope "/", VitalWeb do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index

    get "/signin"           , SignInController, :index
    post "/signin"          , SignInController, :signin
    post "/signin/register" , SignInController, :register
  end

  scope "/api", VitalWeb do
    pipe_through :api
  end
end
