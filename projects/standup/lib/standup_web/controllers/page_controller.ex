defmodule StandupWeb.PageController do
  use StandupWeb, :controller

  @todo [
    "new user experience (create author record)",
    "create journal",
    "create entry",
    "index listing page",
    "user detail page",
    "hook up mailer for login (SMTP/mailgun?)",
    "support oauth providers?",
  ]

  def index(conn, _params) do
    render(conn, "index.html", todo: @todo)
  end
end
