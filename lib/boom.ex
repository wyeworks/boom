defmodule Boom do
  @moduledoc false
  # Notify the exception to all the defined notifiers

  alias Boom.ErrorStorage

  defmacro __using__(config) do
    quote location: :keep do
      use Plug.ErrorHandler

      import Boom
      require Logger

      def handle_errors(conn, error) do
        {error_kind, error_info} = ErrorInfo.build(error, conn)

        ErrorStorage.add_errors(error_kind, error_info)

        if ErrorStorage.send_notification?(error_kind) do
          occurrences = ErrorStorage.get_errors(error_kind)

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

          ErrorStorage.clear_errors(notification_trigger, error_kind)
        end
      rescue
        e ->
          error_info = Exception.format_banner(:error, e, __STACKTRACE__)

          [first_stack_entry | _] = __STACKTRACE__

          # Will transform '(boom) test/notifier_test.exs:32: NotifierTest.FailingNotifier.notify/2'
          # into 'NotifierTest.FailingNotifier.notify/2'
          [_failing_notifier_file, failing_notifier] =
            first_stack_entry
            |> Exception.format_stacktrace_entry()
            |> String.split(~r{:\d+:\s})

          Logger.warn(
            "An error occurred when sending a notification: #{error_info} in #{failing_notifier}"
          )
      end
    end
  end
end
