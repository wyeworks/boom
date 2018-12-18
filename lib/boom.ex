defmodule Boom do
  defmacro __using__(_) do
    quote location: :keep do
      use Plug.ErrorHandler
      import Boom
      import Bamboo.Email

      defp handle_errors(conn, %{reason: exception}) do
        try do
          welcome_email = new_email
            |> to("foo@example.com")
            |> from("me@example.com")
            |> subject("Welcome!!!")
            |> html_body("<strong>Welcome</strong>")
            |> text_body("welcome")

          welcome_email |> mailer.deliver_now
        rescue
          e -> IO.inspect(e, label: "[Boom] Error sending exception")
        end
      end

      defp mailer do
        Application.fetch_env!(:boom, :mailer)
      end
    end
  end
end
