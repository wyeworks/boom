defmodule Boom do
  defmacro __using__(_) do
    quote location: :keep do
      use Plug.ErrorHandler
      import Boom

      defp handle_errors(conn, %{reason: exception}) do
        IO.inspect exception, label: "Boom library output hackaton"
      end
    end
  end
end
