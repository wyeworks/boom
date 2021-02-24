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
        for notifier_settings <- notifiers_settings do
          run_callback(notifier_settings, callback)
        end
    end
  end

  defmacro __using__(config) do
    quote location: :keep do
      use Plug.ErrorHandler

      import BoomNotifier
      require Logger

      settings = unquote(config)

      walkthrough_notifiers(
        settings,
        &if function_exported?(&1, :validate_config, 1) do
          with {:error, message} <- &1.validate_config(&2) do
            Logger.error(
              "Notifier validation: #{message} in #{
                &1 |> to_string() |> String.split(".") |> List.last()
              }"
            )
          end
        end
      )

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

          Logger.error(
            "An error occurred when sending a notification: #{error_info} in #{failing_notifier}"
          )
      end
    end
  end
end
