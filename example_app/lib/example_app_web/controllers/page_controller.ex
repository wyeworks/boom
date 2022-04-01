defmodule GroupExceptionError do
  defexception message: "group exception error"
end

defmodule IgnoreExceptionError do
  defexception message: "ignore exception error"
end

defmodule ExampleAppWeb.PageController do
  use ExampleAppWeb, :controller

  def index(conn, _params) do
    raise "Boom"
    render(conn, "index.html")
  end

  def group_exception(conn, _params) do
    raise GroupExceptionError
    render(conn, "index.html")
  end

  def ignore_exception(conn, _params) do
    raise IgnoreExceptionError
    render(conn, "index.html")
  end
end
