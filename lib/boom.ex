defmodule Boom do
  @moduledoc false
  # Notify the exception to all the defined notifiers

  alias Boom.ErrorStorage

  defmacro __using__(config) do
    quote location: :keep do
      use Plug.ErrorHandler

      import Boom

      def handle_errors(conn, %{reason: reason, stack: stack} = error) do
        error_reason = ErrorInfo.get_reason(error)
        error_info = ErrorInfo.build(reason, stack, conn)

        ErrorStorage.add_errors(error_reason, error_info)

        if ErrorStorage.send_notification?(error_reason) do
          occurrences = ErrorStorage.get_errors(error_reason)

          settings = unquote(config)

          case Keyword.get(settings, :notifiers) do
            nil ->
              with {:ok, notifier} <- Keyword.fetch(settings, :notifier),
                   {:ok, options} <- Keyword.fetch(settings, :options) do
                notifier.notify(occurrences, options)
              end

            notifiers_config when is_list(notifiers_config) ->
              for notifier_config <- notifiers_config do
                with {:ok, notifier} <- Keyword.fetch(notifier_config, :notifier),
                     {:ok, options} <- Keyword.fetch(notifier_config, :options) do
                  notifier.notify(occurrences, options)
                end
              end
          end

          {notification_trigger, _settings} =
            Keyword.pop(settings, :notification_trigger, :always)

          ErrorStorage.clear_errors(notification_trigger, error_reason)
        end
      rescue
        # FIXME: we should handle this in a different way
        # credo:disable-for-next-line
        e -> IO.inspect(e, label: "[Boom] Error sending exception")
      end
    end
  end
end
