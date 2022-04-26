defmodule CustomExceptionError do
  defexception message: "custom exception error"
end

defmodule GroupExceptionError do
  defexception message: "group exception error"
end

defmodule IgnoreExceptionError do
  defexception message: "ignore exception error"
end

defmodule CustomNotifierExceptionError do
  defexception message: "custom notifier exception error"
end

defmodule ExampleAppWeb.PageController do
  use ExampleAppWeb, :controller

  def index(conn, _params) do
    raise CustomExceptionError
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

  def custom_notifier_exception(conn, _params) do
    raise CustomNotifierExceptionError
    render(conn, "index.html")
  end

  def check_custom_notifier(conn, _params) do
    error = ExampleApp.MemoryErrorStorage.get_error()
    render(conn, "check_custom_notifier.html", error: error)
  end
end
