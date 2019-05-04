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
        request_info_lines = Enum.slice(body, 1..4)

        assert [
                 "Request Information:\n",
                 "Path: /\n",
                 "Method: GET\n",
                 "URL: http://www.example.com/\n"
               ] = request_info_lines
    end
  end

  # test "Set email HTML body with controller/action where error happened" do
  #   conn = conn(:get, "/")
  #   catch_error(TestRouter.call(conn, []))

  #   assert_received {:email_html_body,
  #                    "<p>TestException occurred while the request was processed by TestController#index</p>" <>
  #                      _}
  # end
end
