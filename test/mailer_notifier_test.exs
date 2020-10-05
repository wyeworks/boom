defmodule MailerNotifierTest do
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

  test "Raising an error on failure" do
    conn = conn(:get, "/")

    assert_raise Plug.Conn.WrapperError,
                 "** (MailerNotifierTest.TestController.TestException) booom!",
                 fn ->
                   TestRouter.call(conn, [])
                 end
  end

  test "Set email subject including exception message" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    assert_received {:email_subject, "BOOM error caught: booom!"}
  end

  test "Set email using proper from and to addresses" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    assert_received {:email_from, "me@example.com"}
    assert_received {:email_to, "foo@example.com"}
  end

  test "Exception summary is the first part of email text body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    assert_received {:email_text_body,
                     [
                       "TestException occurred while the request was processed by TestController#index"
                       | _
                     ]}
  end

  test "Exception summary is the first part of email HTML body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    assert_received {:email_html_body,
                     "\n  <p>TestException occurred while the request was processed by TestController#index</p>" <>
                       _}
  end

  test "Request information is part of the email text body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    receive do
      {:email_text_body, body} ->
        request_info_lines = Enum.slice(body, 1..8)

        assert [
                 "Request Information:",
                 "URL: http://www.example.com/",
                 "Path: /",
                 "Method: GET",
                 "Port: 80",
                 "Scheme: http",
                 "Query String:",
                 "Client IP: 127.0.0.1"
               ] = request_info_lines
    end
  end

  test "Request information is part of the email HTML body" do
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

  test "Exception stacktrace appears in email text body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    receive do
      {:email_text_body, body} ->
        first_stack_line = Enum.at(body, 10)

        assert "test/mailer_notifier_test.exs:" <>
                 <<name::binary-size(2), ": MailerNotifierTest.TestController.index/2">> =
                 first_stack_line
    end
  end

  test "Exception stacktrace appears in email HTML body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    receive do
      {:email_html_body, body} ->
        [stacktrace_list | _] =
          Regex.scan(~r/<ul.+?>(.)+?<\/ul>/s, body)
          |> Enum.at(2)

        [first_stack_line | _] =
          Regex.scan(~r/<li>(.)+?<\/li>/s, stacktrace_list)
          |> Enum.map(&Enum.at(&1, 0))

        [file, exception] =
          Regex.scan(~r/<span.*>(.+)<\/span>/, first_stack_line)
          |> Enum.map(&Enum.at(&1, 1))

        assert "test/mailer_notifier_test.exs:16" = file
        assert "Elixir.MailerNotifierTest.TestController.index/2" = exception
    end
  end

  test "Exception timestamp appears in email text body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    receive do
      {:email_text_body, body} ->
        timestamp_line = Enum.at(body, 9)

        assert timestamp_line =~ ~r/Occurred on: \d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}/
    end
  end

  test "Exception timestamp appears in email HTML body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    receive do
      {:email_html_body, body} ->
        [timestamp_list | _] =
          Regex.scan(~r/<ul.+?>(.)+?<\/ul>/s, body)
          |> Enum.at(1)

        [timestamp_line | _] =
          Regex.scan(~r/<li>(.)+?<\/li>/s, timestamp_list)
          |> Enum.map(&Enum.at(&1, 0))

        assert timestamp_line =~ ~r/Occurred on: \d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}/
    end
  end
end
