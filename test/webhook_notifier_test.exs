defmodule WebhookNotifierTest do
  use ExUnit.Case, async: true
  use Plug.Test

  doctest BoomNotifier

  @expected_response %{
    exception_summary:
      "TestException occurred while the request was processed by TestController#index",
    exception_stack_entries: [
      "test/webhook_notifier_test.exs:44: WebhookNotifierTest.TestController.index/2"
    ],
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

    use BoomNotifier,
      notifier: BoomNotifier.WebhookNotifier,
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

  test "validates raises an exception when url is not present" do
    assert_raise KeyError, "key :url not found in: [random_param: nil]", fn ->
      BoomNotifier.WebhookNotifier.validate!(random_param: nil)
    end
  end

  test "request is sent to webhook", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      assert "POST" == conn.method
      {:ok, body, _conn} = Plug.Conn.read_body(conn)

      [
        %{
          exception_summary: exception_summary,
          exception_stack_entries: [first_stack_entry | _] = exception_stack_entries,
          request: request,
          timestamp: timestamp
        }
      ] = Jason.decode!(body, keys: :atoms)

      assert exception_summary == @expected_response.exception_summary

      assert length(exception_stack_entries) == 9
      assert first_stack_entry =~ "WebhookNotifierTest.TestController.index/2"

      assert request == @expected_response.request

      {:ok, timestamp, _utc_offset} = DateTime.from_iso8601(timestamp)
      assert DateTime.diff(timestamp, DateTime.utc_now(), :second) <= 1

      Plug.Conn.resp(conn, 200, [])
    end)

    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))
  end
end
