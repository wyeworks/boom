defmodule BoomNotifier do
  @moduledoc false

  # Notify the exception to all the defined notifiers

  alias BoomNotifier.ErrorStorage
  require Logger

  def run_callback(settings, callback) do
    missing_keys = Enum.reject([:notifier, :options], &Keyword.has_key?(settings, &1))

    case missing_keys do
      [] ->
        callback.(settings[:notifier], settings[:options])

      [missing_key] ->
        Logger.error("Settings error: #{inspect(missing_key)} parameter missing")

      _ ->
        Logger.error(
          "Settings error: The following parameters are missing: #{inspect(missing_keys)}"
        )
    end
  end

  def walkthrough_notifiers(settings, callback) do
    case Keyword.get(settings, :notifiers) do
      nil ->
        run_callback(settings, callback)

      notifiers_settings when is_list(notifiers_settings) ->
        Enum.each(notifiers_settings, &run_callback(&1, callback))
    end
  end

  def validate_notifiers(notifier, options) do
    if Code.ensure_loaded?(notifier) && function_exported?(notifier, :validate_config, 1) do
      case notifier.validate_config(options) do
        {:error, message} ->
          Logger.error(
            "Notifier validation: #{message} in #{
              notifier |> to_string() |> String.split(".") |> List.last()
            }"
          )

        _ ->
          nil
      end
    end
  end

  defmacro __using__(config) do
    quote location: :keep do
      use Plug.ErrorHandler

      import BoomNotifier
      require Logger

      settings = unquote(config)

      # Notifiers validation
      walkthrough_notifiers(
        settings,
        fn notifier, options -> validate_notifiers(notifier, options) end
      )

      def handle_errors(conn, error) do
        settings = unquote(config)

        {custom_data, _settings} = Keyword.pop(settings, :custom_data, :nothing)

        {error_kind, error_info} = ErrorInfo.build(error, conn, custom_data)

        ErrorStorage.add_errors(error_kind, error_info)

        if ErrorStorage.send_notification?(error_kind) do
          occurrences = ErrorStorage.get_errors(error_kind)

          # Triggers the notification in each notifier
          walkthrough_notifiers(settings, fn notifier, options ->
            notifier.notify(occurrences, options)
          end)

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

          Logger.error(
            "An error occurred when sending a notification: #{error_info} in #{failing_notifier}"
          )
      end
    end
  end
end
