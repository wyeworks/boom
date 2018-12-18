defmodule Boom do
  defmacro __using__(_) do
    quote location: :keep do
      use Plug.ErrorHandler
      import Boom
      import Bamboo.Email

      defp handle_errors(conn, %{reason: exception}) do
        IO.inspect exception, label: "Boom library output hackaton"

        welcome_email = new_email
          |> to("foo@example.com")
          |> from("me@example.com")
          |> subject("Welcome!!!")
          |> html_body("<strong>Welcome</strong>")
          |> text_body("welcome")

        welcome_email |> mailer.deliver_now
      end

      defp mailer do
        Application.get_env(:boom, :mailer)
      end
    end
  end
end
