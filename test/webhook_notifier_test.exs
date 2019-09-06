defmodule WebhookNotifierTest do
  use ExUnit.Case, async: true
  use Plug.Test

  doctest Boom

  @expected_response %{
                       exception_stack_entries: [
                         "test/webhook_notifier_test.exs:42: WebhookNotifierTest.TestController.index/2",
                         "test/webhook_notifier_test.exs:33: WebhookNotifierTest.TestController.action/2",
                         "test/webhook_notifier_test.exs:33: WebhookNotifierTest.TestController.phoenix_controller_pipeline/2",
                         "(phoenix) lib/phoenix/router.ex:275: Phoenix.Router.__call__/1",
                         "lib/plug/error_handler.ex:64: WebhookNotifierTest.TestRouter.call/2",
                         "test/webhook_notifier_test.exs:78: WebhookNotifierTest.\"test request is sent to webhook\"/1",
                         "(ex_unit) lib/ex_unit/runner.ex:355: ExUnit.Runner.exec_test/1",
                         "(stdlib) timer.erl:166: :timer.tc/1",
                         "(ex_unit) lib/ex_unit/runner.ex:306: anonymous fn/4 in ExUnit.Runner.spawn_test_monitor/4"
                       ],
                       exception_summary:
                         "TestException occurred while the request was processed by TestController#index",
                       request: %{
                         client_ip: "127.0.0.1",
                         method: "GET",
                         path: "/",
                         port: 80,
                         query_string: "",
                         scheme: "http",
                         url: "http://www.example.com/"
                       }
                     }
                     |> Poison.encode!()

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

  defmodule TestRouter do
    use Phoenix.Router
    import Phoenix.Controller

    use Boom,
      notifier: Boom.WebhookNotifier,
      options: [url: "http://localhost:1234"]

    pipeline :browser do
      plug(:accepts, ["html"])
    end

    scope "/" do
      pipe_through(:browser)
      get("/", TestController, :index)
    end
  end

  setup do
    bypass = Bypass.open(port: 1234)
    {:ok, bypass: bypass}
  end

  test "request is sent to webhook", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      assert "POST" == conn.method
      {:ok, body, _conn} = Plug.Conn.read_body(conn)
      assert body == @expected_response
      Plug.Conn.resp(conn, 200, [])
    end)

    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))
  end
end
