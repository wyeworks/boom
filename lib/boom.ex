defmodule Boom do
  @moduledoc false
  # Notify the exception to all the defined notifiers

  defmacro __using__(config) do
    quote location: :keep do
      use Plug.ErrorHandler

      import Boom

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
          # FIXME: we should handle this in a different way
          # credo:disable-for-next-line
          e -> IO.inspect(e, label: "[Boom] Error sending exception")
        end
      end
    end
  end
end
