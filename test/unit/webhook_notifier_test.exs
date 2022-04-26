defmodule WebhookNotifierTest do
  use ExUnit.Case, async: false
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
    },
    metadata: %{
      assigns: %{
        name: "Davis",
        age: 32
      },
      logger: %{
        name: "Dennis",
        age: 17
      }
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
      options: [
        url: "http://localhost:1234",
        headers: [Authorization: "Bearer token123"]
      ],
      custom_data: [:assigns, :logger]

    pipeline :browser do
      plug(:accepts, ["html"])
      plug(:save_custom_data)
    end

    scope "/" do
      pipe_through(:browser)
      get("/", TestController, :index, log: false)
    end

    def save_custom_data(conn, _) do
      conn
      |> assign(:name, "Davis")
      |> assign(:age, 32)
    end
  end

  def header_value(headers, header_name) do
    Enum.find(headers, fn {header, _value} -> header == header_name end)
    |> elem(1)
  end

  setup do
    Logger.metadata(name: "Dennis", age: 17)
    bypass = Bypass.open(port: 1234)
    {:ok, bypass: bypass}
  end

  test "validates return {:error, message} when url is not present" do
    assert {:error, ":url parameter is missing"} ==
             BoomNotifier.WebhookNotifier.validate_config(random_param: nil)
  end

  test "request is sent to webhook", %{bypass: bypass} do
    Bypass.expect(bypass, fn conn ->
      assert "POST" == conn.method

      assert header_value(conn.req_headers, "authorization") == "Bearer token123"
      assert header_value(conn.req_headers, "content-type") == "application/json"

      {:ok, body, _conn} = Plug.Conn.read_body(conn)

      %{
        exception_summary: exception_summary,
        exception_stack_entries: [first_stack_entry | _] = exception_stack_entries,
        request: request,
        timestamp: timestamp,
        metadata: metadata
      } = Jason.decode!(body, keys: :atoms)

      assert exception_summary == @expected_response.exception_summary

      assert length(exception_stack_entries) == 9
      assert first_stack_entry =~ "WebhookNotifierTest.TestController.index/2"

      assert request == @expected_response.request
      assert metadata == @expected_response.metadata

      {:ok, timestamp, _utc_offset} = DateTime.from_iso8601(timestamp)
      assert DateTime.diff(timestamp, DateTime.utc_now(), :second) <= 1

      Plug.Conn.resp(conn, 200, [])
    end)

    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    # Wait for the background request to be sent
    Process.sleep(500)
  end
end
