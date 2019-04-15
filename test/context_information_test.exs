defmodule ContextInformationTest do
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

  test "Set email subject including exception message" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    assert_received {:email_subject, "BOOM error caught: booom!"}
  end

  test "Set email text body with controller where error happened" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    assert_received {:email_text_body,
                     [
                       "Controller: TestController\n"
                       | _
                     ]}
  end

  test "Set email HTML body with controller where error happened" do
    conn = conn(:get, "/")
    catch_error(TestRouter.call(conn, []))

    assert_received {:email_html_body, "<p>Controller: TestController</p>" <> _}
  end
end
