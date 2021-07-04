defmodule MailerNotifierTest do
  use ExUnit.Case
  use Plug.Test

  doctest BoomNotifier

  @receive_timeout 100

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
      notifier: BoomNotifier.MailNotifier,
      options: [
        mailer: Support.FakeMailer,
        from: "me@example.com",
        to: self(),
        subject: "BOOM error caught"
      ],
      custom_data: [:assigns, :logger]

    pipeline :browser do
      plug(:accepts, ["html"])
      plug(:save_custom_data)
    end

    scope "/" do
      pipe_through(:browser)
      get("/", TestController, :index)
    end

    def save_custom_data(conn, _) do
      conn
      |> assign(:name, "Davis")
      |> assign(:age, 32)
    end
  end

  setup do
    Logger.metadata(name: "Dennis", age: 17)
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

    assert_receive({:email_subject, "BOOM error caught: booom!"}, @receive_timeout)
  end

  test "Set email using proper from and to addresses" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))
    email_to = inspect(self())

    assert_receive({:email_from, "me@example.com"}, @receive_timeout)
    assert_receive({:email_to, email_to}, @receive_timeout)
  end

  test "Exception summary is the first part of email text body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    assert_receive(
      {:email_text_body,
       [
         "TestException occurred while the request was processed by TestController#index"
         | _
       ]},
      @receive_timeout
    )
  end

  test "Exception summary is the first part of email HTML body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    assert_receive(
      {:email_html_body,
       "\n  <p>TestException occurred while the request was processed by TestController#index</p>" <>
         _},
      @receive_timeout
    )
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
        first_stack_line = Enum.at(body, 17)

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
          |> Enum.at(4)

        [first_stack_line | _] =
          Regex.scan(~r/<li>(.)+?<\/li>/s, stacktrace_list)
          |> Enum.map(&Enum.at(&1, 0))

        [file, exception] =
          Regex.scan(~r/<span.*>(.+)<\/span>/, first_stack_line)
          |> Enum.map(&Enum.at(&1, 1))

        assert "test/mailer_notifier_test.exs:18" = file
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

  test "validates return {:error, message} when required params are not present" do
    assert {:error, "The following parameters are missing: [:mailer, :from, :to, :subject]"} ==
             BoomNotifier.MailNotifier.validate_config(random_param: nil)

    assert {:error, "The following parameters are missing: [:from, :to, :subject]"} ==
             BoomNotifier.MailNotifier.validate_config(mailer: nil, random_param: nil)

    assert {:error, "The following parameters are missing: [:to, :subject]"} ==
             BoomNotifier.MailNotifier.validate_config(
               mailer: nil,
               from: nil,
               random_param: nil
             )

    assert {:error, ":subject parameter is missing"} ==
             BoomNotifier.MailNotifier.validate_config(
               mailer: nil,
               from: nil,
               to: nil,
               random_param: nil
             )
  end

  test "Custom data appears in email text body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    receive do
      {:email_text_body, body} ->
        custom_data_info = Enum.slice(body, 10..16)

        assert [
                 "Metadata:",
                 "assigns:",
                 "age: 32",
                 "name: Davis",
                 "logger:",
                 "age: 17",
                 "name: Dennis"
               ] = custom_data_info
    end
  end

  test "Custom data appears in email HTML body" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    receive do
      {:email_html_body, body} ->
        custom_data_info =
          Regex.scan(~r/<li>(.)+?<\/li>/, body)
          |> Enum.map(&Enum.at(&1, 0))
          |> Enum.slice(9..13)

        assert [
                 "<li>age: 32 </li>",
                 "<li>name: Davis </li>",
                 "<li>age: 17 </li>",
                 "<li>name: Dennis </li>"
               ] = custom_data_info
    end
  end
end
