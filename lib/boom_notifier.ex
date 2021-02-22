defmodule BoomNotifier do
  @moduledoc false

  # Notify the exception to all the defined notifiers

  alias BoomNotifier.ErrorStorage

  def walkthrough_notifiers(settings, callback) do
    case Keyword.get(settings, :notifiers) do
      nil ->
        with {:ok, notifier} <- Keyword.fetch(settings, :notifier),
             {:ok, options} <- Keyword.fetch(settings, :options) do
          callback.(notifier, options)
        end

      notifiers_config when is_list(notifiers_config) ->
        for notifier_config <- notifiers_config do
          with {:ok, notifier} <- Keyword.fetch(notifier_config, :notifier),
               {:ok, options} <- Keyword.fetch(notifier_config, :options) do
            callback.(notifier, options)
          end
        end
    end
  end

  defmacro __using__(config) do
    quote location: :keep do
      use Plug.ErrorHandler

      import BoomNotifier
      require Logger

      settings = unquote(config)

      try do
        walkthrough_notifiers(
          settings,
          &if(function_exported?(&1, :validate!, 1)) do
            &1.validate!(&2)
          end
        )
      rescue
        e ->
          error_info = Exception.format_banner(:error, e, __STACKTRACE__)
          [{module, function, arity, _}, {notifier, _, _, _} | _] = __STACKTRACE__
          Exception.format_mfa(module, function, arity)

          Logger.warn(
            "Missing parameter: #{error_info} in #{
              notifier |> to_string() |> String.split(".") |> List.last()
            }"
          )
      end

      def handle_errors(conn, error) do
        {error_kind, error_info} = ErrorInfo.build(error, conn)

        ErrorStorage.add_errors(error_kind, error_info)

        if ErrorStorage.send_notification?(error_kind) do
          occurrences = ErrorStorage.get_errors(error_kind)

          settings = unquote(config)

          walkthrough_notifiers(settings, & &1.notify(occurrences, &2))

          {notification_trigger, _settings} =
            Keyword.pop(settings, :notification_trigger, :always)

          ErrorStorage.clear_errors(notification_trigger, error_kind)
        end
      rescue
        e ->
          error_info = Exception.format_banner(:error, e, __STACKTRACE__)

          failing_notifier =
            case __STACKTRACE__ do
              [{module, function, arity, _} | _] ->
                Exception.format_mfa(module, function, arity)

              [first_stack_entry | _] ->
                Exception.format_stacktrace_entry(first_stack_entry)
            end

          Logger.warn(
            "An error occurred when sending a notification: #{error_info} in #{failing_notifier}"
          )
      end
    end
  end
end
