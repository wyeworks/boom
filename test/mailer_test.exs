defmodule MailerTest do
  use ExUnit.Case
  use Plug.Test

  doctest Boom

  defmodule FakeMailer do
    def deliver_now(email) do
      send self(), {:email_subject, email.subject}
    end
  end

  defmodule TestException do
    defexception plug_status: 403, message: "booom!"
  end

  defmodule TestPlug do
    use Boom, mailer: FakeMailer

    def call(conn, _opts) do
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
    catch_error TestPlug.call(conn, [])

    assert_received {:email_subject, "BOOM error caught: booom!"}
  end
end
