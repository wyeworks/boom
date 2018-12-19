defmodule Boom do
  defmacro __using__(_) do
    quote location: :keep do
      use Plug.ErrorHandler
      import Boom
      alias Boom.MailerHelper

      defp handle_errors(conn, %{reason: reason, stack: stack}) do
        try do
          MailerHelper.notify(reason, stack)
        rescue
          e -> IO.inspect(e, label: "[Boom] Error sending exception")
        end
      end
    end
  end
end
