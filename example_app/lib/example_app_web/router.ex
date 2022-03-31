defmodule ExampleAppWeb.Router do
  use ExampleAppWeb, :router

  use BoomNotifier,
    notifier: BoomNotifier.MailNotifier.Swoosh,
    notification_trigger: [exponential: [limit: 8]],
    options: [
      mailer: ExampleApp.Mailer,
      from: "me@example.com",
      to: "foo@example.com",
      subject: "BOOM error caught"
    ]

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {ExampleAppWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", ExampleAppWeb do
    pipe_through :browser

    get "/", PageController, :index
  end

  forward "/mailbox", Plug.Swoosh.MailboxPreview

  # Other scopes may use custom stacks.
  # scope "/api", ExampleAppWeb do
  #   pipe_through :api
  # end
end
