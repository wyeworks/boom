defmodule BoomNotifier do
  @moduledoc false

  # Responsible for sending a notification to each notifier every time an
  # exception is raised.

  alias BoomNotifier.ErrorStorage
  alias BoomNotifier.NotifierSenderServer
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
            "Notifier validation: #{message} in #{notifier |> to_string() |> String.split(".") |> List.last()}"
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

      settings = unquote(config)

      # Notifiers validation
      walkthrough_notifiers(
        settings,
        fn notifier, options -> validate_notifiers(notifier, options) end
      )

      def handle_errors(conn, error) do
        settings = unquote(config)

        {custom_data, _settings} = Keyword.pop(settings, :custom_data, :nothing)

        error_info = ErrorInfo.build(error, conn, custom_data)

        ErrorStorage.add_error(error_info)

        if ErrorStorage.send_notification?(error_info) do
          error_data = ErrorStorage.get_error_storage_item(error_info)
          occurrence = Map.merge(error_info, error_data)

          # Triggers the notification for each notifier
          walkthrough_notifiers(settings, fn notifier, options ->
            NotifierSenderServer.send(notifier, occurrence, options)
          end)

          {notification_trigger, _settings} =
            Keyword.pop(settings, :notification_trigger, :always)

          ErrorStorage.reset_accumulated_errors(notification_trigger, error_info)
        end
      end
    end
  end
end
