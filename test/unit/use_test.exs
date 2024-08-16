defmodule BoomNotifier.UseTest do
  use ExUnit.Case

  import TestUtils

  @already_sent {:plug_conn, :sent}

  defmodule Notifier do
    def notify(%{name: name}, _) do
      pid = Process.whereis(BoomNotifier.UseTest)
      send(pid, {:notification_sent, name})
    end
  end

  defmodule TestException do
    defexception [:message]
  end

  setup do
    # Even though tests are run sequentially a test example
    # pid might ocationally be still alive when the next test
    # example starts
    register_pid_override(self(), BoomNotifier.UseTest)

    %{conn: Plug.Test.conn(:get, "/") |> Map.put(:owner, self())}
  end

  def assert_notification_sent(plug, conn) do
    assert_raise(Plug.Conn.WrapperError, fn ->
      plug.call(conn, [])
    end)

    assert_receive({:notification_sent, TestException})
  end

  describe "plug app" do
    defmodule TestEndpointWithoutErrorHandler do
      @moduledoc """
      Plug app
      """
      use Plug.Router
      use BoomNotifier, notifiers: [[notifier: Notifier, options: []]]

      plug(:match)
      plug(:dispatch)

      get("/", do: raise(TestException, "Something went wrong at #{conn.request_path}"))
    end

    test "notifies on exception and does not send a response", %{
      conn: conn
    } do
      assert_notification_sent(TestEndpointWithoutErrorHandler, conn)

      refute_receive(@already_sent)
    end
  end

  describe "plug app with Plug.ErrorHandler" do
    defmodule TestEndpointWithErrorHandler do
      @moduledoc """
      Plug app with Plug.ErrorHandler
      """
      use Plug.Router
      use Plug.ErrorHandler
      use BoomNotifier, notifiers: [[notifier: Notifier, options: []]]

      plug(:match)
      plug(:dispatch)

      get("/", do: raise(TestException, "Something went wrong at #{conn.request_path}"))
    end

    test "notifies on exception and sends a response", %{conn: conn} do
      assert_notification_sent(TestEndpointWithErrorHandler, conn)

      assert_receive(@already_sent)
    end
  end

  describe "phoenix app" do
    defmodule TestPhoenixRouter do
      @moduledoc """
      Phoenix app
      """
      use Phoenix.Router
      import Phoenix.Controller

      use BoomNotifier, notifiers: [[notifier: Notifier, options: []]]

      get("/", TestPhoenixRouter.TestController, :index)
    end

    defmodule TestPhoenixRouter.TestController do
      use Phoenix.Controller, namespace: TestPhoenixRouter

      def index(_conn, _params) do
        raise TestException, "Something went wrong"
      end
    end

    test "notifies on exception", %{conn: conn} do
      assert_notification_sent(TestPhoenixRouter, conn)

      refute_receive(@already_sent)
    end
  end

  describe "phoenix app with Plug.ErrorHandler" do
    defmodule TestPhoenixRouterWithErrorHandler do
      @moduledoc """
      Phoenix app with Plug.ErrorHandler
      """
      use Phoenix.Router
      import Phoenix.Controller
      use Plug.ErrorHandler

      use BoomNotifier, notifiers: [[notifier: Notifier, options: []]]

      get("/", TestPhoenixRouterWithErrorHandler.TestController, :index)
    end

    defmodule TestPhoenixRouterWithErrorHandler.TestController do
      use Phoenix.Controller, namespace: TestPhoenixRouterWithErrorHandler

      def index(_conn, _params) do
        raise TestException, "Something went wrong"
      end
    end

    test "notifies on exception", %{conn: conn} do
      assert_notification_sent(TestPhoenixRouterWithErrorHandler, conn)

      assert_receive(@already_sent)
    end
  end

  describe "phoenix app with Plug.ErrorHandler and handle_errors/2" do
    defmodule TestPhoenixRouterWithErrorHandler2 do
      @moduledoc """
      Phoenix app with Plug.ErrorHandler and custom handle_errors/2
      """
      use Phoenix.Router
      import Phoenix.Controller
      use Plug.ErrorHandler

      use BoomNotifier, notifiers: [[notifier: Notifier, options: []]]

      def handle_errors(conn, _error) do
        send_resp(conn, 504, custom_error_message())
      end

      get("/", TestPhoenixRouterWithErrorHandler2.TestController, :index)

      def custom_error_message do
        "custom_error_message"
      end
    end

    defmodule TestPhoenixRouterWithErrorHandler2.TestController do
      use Phoenix.Controller, namespace: TestPhoenixRouterWithErrorHandler2

      def index(_conn, _params) do
        raise TestException, "Something went wrong"
      end
    end

    test "notifies on exception", %{conn: conn} do
      assert_notification_sent(TestPhoenixRouterWithErrorHandler2, conn)

      error_response = TestPhoenixRouterWithErrorHandler2.custom_error_message()
      assert_receive(@already_sent)
      assert_receive({_ref, {504, _, ^error_response}})
    end
  end
end
