defmodule ExampleAppWeb.Router do
  use ExampleAppWeb, :router

  use BoomNotifier,
    notifier: BoomNotifier.MailNotifier.Swoosh,
    notification_trigger: [exponential: [limit: 8]],
    custom_data: [:assigns, :logger],
    ignore_exceptions: [ArithmeticError],
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
    plug :save_custom_data
  end

  def save_custom_data(conn, _) do
    Logger.metadata(logger_metadata: "test_assign_metadata")
    assign(conn, :assign_metadata, "test_assign_metadata")
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", ExampleAppWeb do
    pipe_through :browser

    get "/raise-exception", PageController, :index
    get "/ignore-exception", PageController, :ignore_exception
  end

  forward "/mailbox", Plug.Swoosh.MailboxPreview
end
