defmodule ExampleAppWeb.PageController do
  use ExampleAppWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
