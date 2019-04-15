defmodule Boom do
  defmacro __using__(config) do
    quote location: :keep do
      import Boom

      use Plug.ErrorHandler

      defp handle_errors(conn, %{reason: reason, stack: stack}) do
        try do
          error_info = build_error_info(reason, stack, conn)

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

      # TODO use a ErrorInfo module
      defp build_error_info(%{message: reason}, stack, conn) do
        %{
          reason: reason,
          stack: stack,
          controller: get_in(conn.private, [:phoenix_controller])
        }
      end

      defp build_error_info(reason, stack, conn) when is_binary(reason) do
        build_error_info(%{message: reason}, stack, conn)
      end

      defp build_error_info(reason, stack, conn) do
        build_error_info(%{message: inspect(reason)}, stack, conn)
      end
    end
  end
end
