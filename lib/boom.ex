defmodule Boom do
  defmacro __using__(config) do
    quote location: :keep do
      import Boom

      use Plug.ErrorHandler

      defp handle_errors(conn, %{reason: reason, stack: stack}) do
        try do
          error_info = ErrorInfo.build(reason, stack, conn)

          case unquote(config) do
            [notifier: notifier, options: options] ->
              notifier.notify(error_info, options)

            notifiers_config when is_list(notifiers_config) ->
              for [notifier: notifier, options: options] <- notifiers_config do
                notifier.notify(error_info, options)
              end
          end
        rescue
          e -> IO.inspect(e, label: "[Boom] Error sending exception")
        end
      end
    end
  end
end
