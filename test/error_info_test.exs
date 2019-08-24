defmodule ErrorInfoTest do
  use ExUnit.Case
  use Phoenix.ConnTest

  defmodule TestController do
    use Phoenix.Controller
    import Plug.Conn

    defmodule TestException do
      defexception plug_status: 403, message: "booom!"
    end

    def index(_conn, _params) do
      raise TestException.exception([])
    end

    def create(_conn, _params) do
      raise TestException.exception([])
    end
  end

  defmodule TestRouter do
    use Phoenix.Router
    import Phoenix.Controller

    use Boom,
      notifier: Boom.MailNotifier,
      options: [
        mailer: Support.FakeMailer,
        from: "me@example.com",
        to: "foo@example.com",
        subject: "BOOM error caught"
      ]

    pipeline :browser do
      plug(:accepts, ["html"])
    end

    scope "/" do
      pipe_through(:browser)
      Phoenix.Router.get("/", TestController, :index)
      Phoenix.Router.post("/create", TestController, :create)
    end
  end

  @endpoint TestRouter

  doctest Boom

  defmodule TestException do
    defexception message: "booom!"
  end

  test "Generic error without exception name" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))
    %ErrorInfo{name: name, reason: reason} = ErrorInfo.build("Boom", [], conn)

    assert "Error" = name
    assert "Boom" = reason
  end

  test "Error without exception name but message" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))
    %ErrorInfo{name: name, reason: reason} = ErrorInfo.build(%{message: "Boom"}, [], conn)

    assert "Error" = name
    assert "Boom" = reason
  end

  test "Error with exception name" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))

    %ErrorInfo{name: name, reason: reason} =
      ErrorInfo.build(%TestException{message: "Boom"}, [], conn)

    assert ErrorInfoTest.TestException = name
    assert "Boom" = reason
  end

  test "Error info includes action" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))
    %ErrorInfo{action: action} = ErrorInfo.build(%TestException{message: "Boom"}, [], conn)

    assert :index = action
  end

  test "Error info includes controller" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))

    %ErrorInfo{controller: controller} =
      ErrorInfo.build(%TestException{message: "Boom"}, [], conn)

    assert TestController = controller
  end

  test "Error info includes request info" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(post(build_conn(), "/create?foo=bar"))
    %ErrorInfo{request: request} = ErrorInfo.build(%TestException{message: "Boom"}, [], conn)

    assert %{
             path: "/create",
             method: "POST",
             url: "http://www.example.com/create?foo=bar",
             port: 80,
             scheme: :http,
             query_string: "foo=bar",
             client_ip: "127.0.0.1"
           } = request
  end

  test "Error info includes stacktrace" do
    %Plug.Conn.WrapperError{conn: conn, stack: stack} = catch_error(get(build_conn(), :index))

    %ErrorInfo{stack: error_info_stack} =
      ErrorInfo.build(%TestException{message: "Boom"}, stack, conn)

    assert {
             ErrorInfoTest.TestController,
             :index,
             2,
             [file: 'test/error_info_test.exs', line: _]
           } = hd(error_info_stack)

    assert {
             ExUnit.Runner,
             _,
             _,
             [file: 'lib/ex_unit/runner.ex', line: _]
           } = List.last(error_info_stack)

    assert 10 = Enum.count(error_info_stack)
  end
end
