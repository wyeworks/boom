defmodule TestController do
  use Phoenix.Controller
  import Plug.Conn

  defmodule TestException do
    defexception plug_status: 403, message: "booom!"
  end

  def index(_conn, _params) do
    raise TestException.exception([])
  end
end
