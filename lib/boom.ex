defmodule Boom do
  @moduledoc false
  # Notify the exception to all the defined notifiers

  alias Boom.ErrorGrouping

  defmacro __using__(config) do
    quote location: :keep do
      use Plug.ErrorHandler

      import Boom

      def handle_errors(conn, %{reason: reason, stack: stack} = error) do
        error_reason = ErrorInfo.get_reason(error)
        error_info = ErrorInfo.build(reason, stack, conn)

        settings = unquote(config)
        {error_grouping, settings} = Keyword.pop(settings, :error_grouping)

        ErrorGrouping.update_errors(error_reason, error_info)

        if ErrorGrouping.send_notification?(error_reason) do
          occurrences = ErrorGrouping.get_errors(error_reason)

          case settings do
            # FIXME: this doesn't match when extra parameters are set
            [notifier: notifier, options: options] ->
              notifier.notify(occurrences, options)

            [notifiers: notifiers_config] when is_list(notifiers_config) ->
              for [notifier: notifier, options: options] <- notifiers_config do
                notifier.notify(occurrences, options)
              end
          end

          ErrorGrouping.clear_errors(error_grouping, error_reason)
        end
      rescue
        # FIXME: we should handle this in a different way
        # credo:disable-for-next-line
        e -> IO.inspect(e, label: "[Boom] Error sending exception")
      end
    end
  end
end
