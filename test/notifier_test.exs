defmodule NotifierTest do
  use ExUnit.Case
  use Plug.Test

  doctest Boom

  defmodule FakeMailer do
    @behaviour Boom.Notifier

    @impl Boom.Notifier
    def create_payload(reason, stack, options) do
      subject_prefix = Keyword.get(options, :subject)

      %{
        email_subject: "#{subject_prefix}: #{reason.message}",
        email_text_body: Enum.map(stack, &(Exception.format_stacktrace_entry(&1) <> "\n")),
        email_from: Keyword.get(options, :from),
        email_to: Keyword.get(options, :to)
      }
    end

    @impl Boom.Notifier
    def notify(payload) do
      send(self(), {:email_subject, payload.email_subject})
      send(self(), {:email_from, payload.email_from})
      send(self(), {:email_to, payload.email_to})
      send(self(), {:email_text_body, payload.email_text_body})
    end
  end

  defmodule TestException do
    defexception plug_status: 403, message: "booom!"
  end

  defmodule TestPlug do
    use Boom,
      notifiers: [
        {
          FakeMailer,
          [from: "me@example.com", to: "foo@example.com", subject: "BOOM error caught"]
        }
      ]

    def call(_conn, _opts) do
      raise TestException.exception([])
    end
  end

  test "Raising an error on failure" do
    conn = conn(:get, "/")

    assert_raise TestException, "booom!", fn ->
      TestPlug.call(conn, [])
    end
  end

  test "Set email subject including exception message" do
    conn = conn(:get, "/")
    catch_error(TestPlug.call(conn, []))

    assert_received {:email_subject, "BOOM error caught: booom!"}
  end

  test "Set email using proper from and to addresses" do
    conn = conn(:get, "/")
    catch_error(TestPlug.call(conn, []))

    assert_received {:email_from, "me@example.com"}
    assert_received {:email_to, "foo@example.com"}
  end

  test "Set email text body with exception stacktrace" do
    conn = conn(:get, "/")
    catch_error(TestPlug.call(conn, []))

    expection_first_line =
      "test/mailer_test.exs:24: MailerTest.TestPlug.\"call (overridable 1)\"/2\n"

    assert_received {:email_text_body, [expection_first_line | _]}
  end
end
