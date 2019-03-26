defmodule Boom do
  defmacro __using__(config) do
    quote location: :keep do
      import Boom

      use Plug.ErrorHandler

      defp handle_errors(_conn, %{reason: reason, stack: stack}) do
        try do
          case unquote(config) do
            [notifier: notifier, options: options] ->
              notifier.notify(reason, stack, options)

            notifiers_config when is_list(notifiers_config) ->
              for [notifier: notifier, options: options] <- notifiers_config do
                notifier.notify(reason, stack, options)
              end
          end
        rescue
          e -> IO.inspect(e, label: "[Boom] Error sending exception")
        end
      end
    end
  end
end
