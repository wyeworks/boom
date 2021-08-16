defmodule ErrorInfoTest do
  use ExUnit.Case

  import Plug.Conn
  import Phoenix.ConnTest

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

    use BoomNotifier,
      notifier: BoomNotifier.MailNotifier,
      options: [
        mailer: Support.FakeMailer,
        from: "me@example.com",
        to: self(),
        subject: "BOOM error caught"
      ]

    pipeline :browser do
      plug(:accepts, ["html"])
      plug(:save_custom_data)
    end

    scope "/" do
      pipe_through(:browser)
      Phoenix.Router.get("/", TestController, :index)
      Phoenix.Router.post("/create", TestController, :create)
    end

    def save_custom_data(conn, _) do
      conn
      |> assign(:name, "Davis")
      |> assign(:age, 32)
    end
  end

  @endpoint TestRouter

  doctest BoomNotifier

  defmodule TestException do
    defexception message: "booom!"
  end

  setup do
    Logger.metadata(name: "Dennis", age: 17)
  end

  test "Generic error without exception name" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))
    error = %{reason: "Boom", stack: []}
    {error_kind, %ErrorInfo{name: name, reason: reason}} = ErrorInfo.build(error, conn, :nothing)

    assert "Error" = name
    assert "Boom" = reason
    assert :error = error_kind
  end

  test "Error without exception name but message" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))
    error = %{reason: %{message: "Boom"}, stack: []}
    {error_kind, %ErrorInfo{name: name, reason: reason}} = ErrorInfo.build(error, conn, :nothing)

    assert "Error" = name
    assert "Boom" = reason
    assert :error = error_kind
  end

  test "Error with exception name" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))

    error = %{reason: %TestException{message: "Boom"}, stack: []}

    {error_kind, %ErrorInfo{name: name, reason: reason}} = ErrorInfo.build(error, conn, :nothing)

    assert ErrorInfoTest.TestException = name
    assert "Boom" = reason
    assert ErrorInfoTest.TestException = error_kind
  end

  test "Error without exception reason but error and kind" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))
    error = %{error: %{kind: :error_kind}, reason: %{message: "Boom"}, stack: []}
    {error_kind, %ErrorInfo{name: name, reason: reason}} = ErrorInfo.build(error, conn, :nothing)

    assert "Error" = name
    assert "Boom" = reason
    assert :error_kind = error_kind
  end

  test "Error info includes action" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{action: action}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: []}, conn, :nothing)

    assert :index = action
  end

  test "Error info includes controller" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{controller: controller}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: []}, conn, :nothing)

    assert TestController = controller
  end

  test "Error info includes request info" do
    %Plug.Conn.WrapperError{conn: conn} = catch_error(post(build_conn(), "/create?foo=bar"))

    {_error_kind, %ErrorInfo{request: request}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: []}, conn, :nothing)

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

    {_error_kind, %ErrorInfo{stack: error_info_stack}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: stack}, conn, :nothing)

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

  test "Error info includes timestamp" do
    %Plug.Conn.WrapperError{conn: conn, stack: stack} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{timestamp: timestamp}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: stack}, conn, :nothing)

    assert DateTime.diff(DateTime.utc_now(), timestamp, :second) <= 1
  end

  test "Error info metadata is nil when strategy is :nothing" do
    %Plug.Conn.WrapperError{conn: conn, stack: stack} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{metadata: metadata}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: stack}, conn, :nothing)

    assert nil == metadata
  end

  test "Error info metadata includes assigns" do
    %Plug.Conn.WrapperError{conn: conn, stack: stack} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{metadata: metadata}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: stack}, conn, :assigns)

    assert %{assigns: %{age: 32, name: "Davis"}} = metadata
  end

  test "Error info metadata includes filtered fields for assigns" do
    %Plug.Conn.WrapperError{conn: conn, stack: stack} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{metadata: metadata}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: stack}, conn,
        assigns: [fields: [:name]]
      )

    assert %{assigns: %{name: "Davis"}} = metadata
  end

  test "Error info metadata includes logger" do
    %Plug.Conn.WrapperError{conn: conn, stack: stack} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{metadata: metadata}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: stack}, conn, :logger)

    assert %{logger: %{age: 17, name: "Dennis"}} = metadata
  end

  test "Error info metadata includes filtered fields for logger" do
    %Plug.Conn.WrapperError{conn: conn, stack: stack} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{metadata: metadata}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: stack}, conn,
        logger: [fields: [:name]]
      )

    assert %{logger: %{name: "Dennis"}} = metadata
  end

  test "Error info metadata includes assigns and logger" do
    %Plug.Conn.WrapperError{conn: conn, stack: stack} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{metadata: metadata}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: stack}, conn, [
        :assigns,
        :logger
      ])

    assert %{
             assigns: %{age: 32, name: "Davis"},
             logger: %{age: 17, name: "Dennis"}
           } = metadata
  end

  test "Error info metadata includes filtered fields for assigns and logger" do
    %Plug.Conn.WrapperError{conn: conn, stack: stack} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{metadata: metadata}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: stack}, conn, [
        [assigns: [fields: [:name]]],
        [logger: [fields: [:age]]]
      ])

    assert %{assigns: %{name: "Davis"}, logger: %{age: 17}} = metadata
  end

  test "Error info metadata includes filtered equal fields for assigns and logger" do
    %Plug.Conn.WrapperError{conn: conn, stack: stack} = catch_error(get(build_conn(), :index))

    {_error_kind, %ErrorInfo{metadata: metadata}} =
      ErrorInfo.build(%{reason: %TestException{message: "Boom"}, stack: stack}, conn, [
        [assigns: [fields: [:name]]],
        [logger: [fields: [:name]]]
      ])

    assert %{assigns: %{name: "Davis"}, logger: %{name: "Dennis"}} = metadata
  end
end
