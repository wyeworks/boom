defmodule RequestInformationTest do
  use ExUnit.Case
  use Plug.Test

  doctest Boom

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
      get("/", TestController, :index)
    end
  end

  test "Set email text body with request information" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    receive do
      {:email_text_body, body} ->
        request_info_lines = Enum.slice(body, 1..8)

        assert [
                 "Request Information:\n",
                 "URL: http://www.example.com/\n",
                 "Path: /\n",
                 "Method: GET\n",
                 "Port: 80\n",
                 "Scheme: http\n",
                 "Query String: \n",
                 "Client IP: 127.0.0.1\n"
               ] = request_info_lines
    end
  end

  test "Set email HTML body with request informationd" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    receive do
      {:email_html_body, body} ->
        request_info_lines =
          Regex.scan(~r/<li>(.)+?<\/li>/, body)
          |> Enum.map(&Enum.at(&1, 0))
          |> Enum.take(8)

        assert [
                 "<li>Request Information:</li>",
                 "<li>URL: http://www.example.com/</li>",
                 "<li>Path: /</li>",
                 "<li>Method: GET</li>",
                 "<li>Port: 80</li>",
                 "<li>Scheme: http</li>",
                 "<li>Query String: </li>",
                 "<li>Client IP: 127.0.0.1</li>"
               ] = request_info_lines
    end
  end
end
