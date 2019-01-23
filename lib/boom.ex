defmodule Boom do
  defmacro __using__(mailer: mailer) do
    quote location: :keep do
      import Boom

      use Plug.ErrorHandler
      alias Boom.MailerHelper

      defp handle_errors(_conn, %{reason: reason, stack: stack}) do
        try do
          MailerHelper.notify(reason, stack, unquote(mailer))
        rescue
          e -> IO.inspect(e, label: "[Boom] Error sending exception")
        end
      end
    end
  end
end
