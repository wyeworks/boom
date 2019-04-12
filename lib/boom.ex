defmodule Boom do
  defmacro __using__(config) do
    quote location: :keep do
      import Boom

      use Plug.ErrorHandler

      defp handle_errors(_conn, %{reason: reason, stack: stack}) do
        try do
          case unquote(config) do
            [notifier: notifier, options: options] ->
              notify(notifier, reason, stack, options)

            notifiers_config when is_list(notifiers_config) ->
              for [notifier: notifier, options: options] <- notifiers_config do
                notify(notifier, reason, stack, options)
              end
          end
        rescue
          e -> IO.inspect(e, label: "[Boom] Error sending exception")
        end
      end

      defp notify(notifier, %{message: reason}, stack, options) do
        notifier.notify(reason, stack, options)
      end

      defp notify(notifier, reason, stack, options) when is_binary(reason) do
        notify(notifier, %{message: reason}, stack, options)
      end

      defp notify(notifier, reason, stack, options) do
        notify(notifier, %{message: inspect(reason)}, stack, options)
      end
    end
  end
end
