defmodule HelloWeb.PageController do
  use HelloWeb, :controller

  def index(conn, _params) do
    raise "Explotoooooo"
    render conn, "index.html"
  end
end
