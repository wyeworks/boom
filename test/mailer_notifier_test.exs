defmodule MailerNotifierTest do
  use ExUnit.Case
  use Plug.Test

  alias BoomNotifier.MailNotifier

  doctest BoomNotifier

  @receive_timeout 500

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

  defmodule TestTemplateErrorController do
    use Phoenix.Controller
    import Plug.Conn

    defmodule TestTemplateErrorException do
      defexception plug_status: 403, message: String.duplicate("a", 300)
    end

    def index(_conn, _params) do
      raise TestTemplateErrorException.exception([])
    end
  end

  # All mail notifiers should behave the same, so define the tests across
  # the set of notifiers.
  #
  # Note: Because `use` is macro, we can't inject `mail_notifier_module`
  #       as the `notifier` value directly, because it wont be resolved
  #       in the correct context. Instead we can define a module attribute
  #       which gets replaced at compile time with correct atom.
  #
  #       We also disable module-redefinition warning just in this context,
  #       since we are redefining the router modules for each notifier.

  notifiers = [
    {"Bamboo", BoomNotifier.MailNotifier.Bamboo, Support.BambooFakeMailer},
    {"Swoosh", BoomNotifier.MailNotifier.Swoosh, Support.SwooshFakeMailer}
  ]

  for {name, mail_notifier_module, fake_mailer_module} <- notifiers do
    describe name do
      # Disable "redefining module" warnings, we have intent.
      :elixir_config.put(:ignore_module_conflict, true)

      defmodule TestRouter do
        use Phoenix.Router
        import Phoenix.Controller

        @mail_notifier_module mail_notifier_module
        @fake_mailer_module fake_mailer_module
        use BoomNotifier,
          notifier: @mail_notifier_module,
          options: [
            mailer: @fake_mailer_module,
            from: "me@example.com",
            to: inspect(self()),
            subject: "BOOM error caught"
          ],
          custom_data: [:assigns, :logger]

        pipeline :browser do
          plug(:accepts, ["html"])
          plug(:save_custom_data)
        end

        scope "/" do
          pipe_through(:browser)
          get("/", TestController, :index, log: false)
          get("/template_error", TestTemplateErrorController, :index, log: false)
        end

        def save_custom_data(conn, _) do
          conn
          |> assign(:name, "Davis")
          |> assign(:age, 32)
        end
      end

      defmodule TestSubjectConfigRouter do
        use Phoenix.Router

        @mail_notifier_module mail_notifier_module
        @fake_mailer_module fake_mailer_module
        use BoomNotifier,
          notifier: @mail_notifier_module,
          options: [
            mailer: @fake_mailer_module,
            from: "me@example.com",
            to: inspect(self()),
            subject: "BOOM error caught",
            max_subject_length: 25
          ]

        scope "/" do
          get("/template_error", TestTemplateErrorController, :index, log: false)
        end
      end

      # Finished with any intentional redefinition.
      :elixir_config.put(:ignore_module_conflict, false)

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

      test "Subject cannot be greater than 25 chars" do
        conn = conn(:get, "/template_error")
        catch_error(TestSubjectConfigRouter.call(conn, []))

        assert_receive({:email_subject, "BOOM error caught: aaaaaa"}, @receive_timeout)
      end

      test "Subject should be truncated to 80 chars as default" do
        conn = conn(:get, "/template_error")
        catch_error(TestRouter.call(conn, []))

        receive do
          {:email_subject, subject} ->
            assert String.length(subject) == 80
        end
      end

      test "Set email using proper from and to addresses" do
        conn = conn(:get, "/")
        catch_error(TestRouter.call(conn, []))
        email_to = self()

        assert_receive({:email_from, "me@example.com"}, @receive_timeout)
        assert_receive({:email_to, ^email_to}, @receive_timeout)
      end

      test "Exception summary is included in the email's text body" do
        conn = conn(:get, "/")
        catch_error(TestRouter.call(conn, []))

        assert_receive(
          {:email_text_body, text_body},
          @receive_timeout
        )

        assert "TestException occurred while the request was processed by TestController#index" in text_body
      end

      test "Exception summary is included in the email's HTML body" do
        conn = conn(:get, "/")
        catch_error(TestRouter.call(conn, []))

        assert_receive(
          {:email_html_body, html_body},
          @receive_timeout
        )

        assert String.contains?(
                 html_body,
                 "TestException occurred while the request was processed by TestController#index"
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
              |> Enum.map(&remove_markup/1)

            assert [
                     "URL: http://www.example.com/",
                     "Path: /",
                     "Method: GET",
                     "Port: 80",
                     "Scheme: http",
                     "Query String: ",
                     "Client IP: 127.0.0.1",
                     "Occurred on: " <> _timestamp
                   ] = request_info_lines
        end
      end

      test "reason appears in email text body" do
        conn = conn(:get, "/template_error")
        catch_error(TestRouter.call(conn, []))

        receive do
          {:email_text_body, body} ->
            reason_lines = Enum.take(body, -3)

            assert [
                     "Reason:",
                     String.duplicate("a", 300),
                     "----------------------------------------"
                   ] == reason_lines
        end
      end

      test "reason appears in email HTML body" do
        conn = conn(:get, "/template_error")
        catch_error(TestRouter.call(conn, []))

        receive do
          {:email_html_body, body} ->
            assert String.contains?(body, ["Reason:", String.duplicate("a", 300)])
        end
      end

      test "Exception stacktrace appears in email text body" do
        conn = conn(:get, "/")
        catch_error(TestRouter.call(conn, []))

        receive do
          {:email_text_body, body} ->
            first_stack_line = Enum.at(body, 17)

            assert "test/mailer_notifier_test.exs:" <>
                     <<_name::binary-size(2), ": MailerNotifierTest.TestController.index/2">> =
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
              |> Enum.at(3)

            [first_stack_line | stacktrace_list] =
              Regex.scan(~r/<li>(.)+?<\/li>/s, stacktrace_list)
              |> Enum.map(&Enum.at(&1, 0))

            [second_stack_line | _] = stacktrace_list

            assert "<li>test/mailer_notifier_test.exs:20: MailerNotifierTest.TestController.index/2</li>" =
                     first_stack_line

            assert "<li>test/mailer_notifier_test.exs:11: MailerNotifierTest.TestController.action/2</li>" =
                     second_stack_line
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
              |> Enum.at(0)

            [timestamp_line] =
              Regex.scan(~r/<li>.+?<\/li>/s, timestamp_list)
              |> List.last()

            clean_timestamp = remove_markup(timestamp_line)

            assert clean_timestamp =~ ~r/Occurred on: \d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}/
        end
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
              Regex.scan(~r/<li>.+?<\/li>/, body)
              |> Enum.map(&Enum.at(&1, 0))
              |> Enum.slice(8..11)

            assert [
                     "<li>age: 32 </li>",
                     "<li>name: Davis </li>",
                     "<li>age: 17 </li>",
                     "<li>name: Dennis </li>"
                   ] = custom_data_info
        end
      end
    end
  end

  describe "MailNotifier.validate_config" do
    setup do
      options = [
        mailer: Support.BambooFakeMailer,
        from: "boom@mailer",
        to: "user@mail",
        subject: "boom mail",
        max_subject_length: 80
      ]

      [options: options]
    end

    test "returns :ok for good config", %{options: opts} do
      assert :ok = MailNotifier.validate_config(opts)
    end

    test "returns {:error, message} when required params are not present" do
      assert {:error, "The following parameters are missing: [:mailer, :from, :to, :subject]"} ==
               MailNotifier.validate_config(random_param: nil)
    end

    test "returns {:error, message} when option value is incorrect type", %{options: opts} do
      bad_addresses = [nil, 1, :"my@mail.com", {nil, nil}, {nil}]

      bad_option_values = %{
        from: bad_addresses,
        # :to can be a list of addresses or one
        to: List.flatten(for addr <- bad_addresses, do: [addr, [addr, addr]]),
        subject: [nil, 1],
        mailer: [nil, :"Elixir.BoomNotifier.MailerNotifier.BadModule"],
        max_subject_length: [nil, -1, "19"]
      }

      for {key, bad_values} <- bad_option_values, val <- bad_values do
        # Options with bad value should return {:error, message}.
        # For nicer errors we will call match? directly and generate a more
        # descriptive assertion message. We wont match {:error, message} on
        # the first call, even though we do later to check the message value
        # because the failure ":ok did not match right side" is unhelpful when
        # debugging tests.

        result = MailNotifier.validate_config(Keyword.put(opts, key, val))

        assert(
          match?({:error, _message}, result),
          "expected #{key}: #{inspect(val)} to return {:error, message}"
        )

        {:error, message} = result
        assert String.starts_with?(message, ":#{key}")
      end
    end

    test ":from option can accept an address or {name, address}", %{options: opts} do
      assert :ok = MailNotifier.validate_config(Keyword.put(opts, :from, "boom@mail"))
      assert :ok = MailNotifier.validate_config(Keyword.put(opts, :from, {"Boom", "boom@mail"}))
      # a user may accidentally think from behaves like to, so make sure we are returning an error
      assert {:error, _message} =
               MailNotifier.validate_config(Keyword.put(opts, :from, [{"Boom", "boom@mail"}]))
    end

    test ":to option can accept an address or {name, address}, or list", %{options: opts} do
      assert :ok = MailNotifier.validate_config(Keyword.put(opts, :to, "esme@mail"))
      assert :ok = MailNotifier.validate_config(Keyword.put(opts, :to, {"Esme", "esme@mail"}))

      assert :ok =
               MailNotifier.validate_config(Keyword.put(opts, :to, ["esme@mail", "obeto@mail"]))

      assert :ok =
               MailNotifier.validate_config(
                 Keyword.put(opts, :to, [
                   {"Esmerelda", "esme@mail"},
                   "obeto@mail"
                 ])
               )
    end
  end

  # strip html tags for content checks
  def remove_markup(string) do
    String.replace(string, ~r/<.+>/U, "")
  end
end
