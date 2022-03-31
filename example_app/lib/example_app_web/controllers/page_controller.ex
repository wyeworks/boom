defmodule ExampleAppWeb.PageController do
  use ExampleAppWeb, :controller

  def index(conn, _params) do
    raise "Boom"
    render(conn, "index.html")
  end

  def ignore_exception(conn, _params) do
    1 / 0
    render(conn, "index.html")
  end
end
