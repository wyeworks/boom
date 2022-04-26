defmodule BoomNotifier do
  @moduledoc false

  # Responsible for sending a notification to each notifier every time an
  # exception is raised.

  alias BoomNotifier.ErrorInfo
  alias BoomNotifier.ErrorStorage
  alias BoomNotifier.NotifierSenderServer
  require Logger

  def run_callback(settings, callback) do
    missing_keys = Enum.reject([:notifier, :options], &Keyword.has_key?(settings, &1))

    case missing_keys do
      [] ->
        callback.(settings[:notifier], settings[:options])

      [missing_key] ->
        Logger.error("(BoomNotifier) #{inspect(missing_key)} parameter is missing")

      _ ->
        Logger.error(
          "(BoomNotifier) The following parameters are missing: #{inspect(missing_keys)}"
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
      import BoomNotifier

      error_handler_in_use = {:handle_errors, 2} in Module.definitions_in(__MODULE__)

      unless error_handler_in_use do
        use Plug.ErrorHandler

        @impl Plug.ErrorHandler
        def handle_errors(conn, error) do
          notify_error(conn, error)
        end
      end

      # Notifiers validation
      walkthrough_notifiers(
        unquote(config),
        fn notifier, options -> validate_notifiers(notifier, options) end
      )

      def notify_error(conn, %{kind: :error, reason: %mod{}} = error) do
        settings = unquote(config)
        {ignored_exceptions, _settings} = Keyword.pop(settings, :ignore_exceptions, [])

        unless Enum.member?(ignored_exceptions, mod) do
          do_notify_error(conn, settings, error)
        end
      end

      def notify_error(conn, error) do
        do_notify_error(conn, unquote(config), error)
      end

      defp do_notify_error(conn, settings, error) do
        {custom_data, _settings} = Keyword.pop(settings, :custom_data, :nothing)
        error_info = ErrorInfo.build(error, conn, custom_data)

        ErrorStorage.store_error(error_info)

        if ErrorStorage.send_notification?(error_info) do
          notification_data =
            Map.put(error_info, :occurrences, ErrorStorage.get_error_stats(error_info))

          # Triggers the notification in each notifier
          walkthrough_notifiers(settings, fn notifier, options ->
            NotifierSenderServer.send(notifier, notification_data, options)
          end)

          {notification_trigger, _settings} =
            Keyword.pop(settings, :notification_trigger, :always)

          ErrorStorage.reset_accumulated_errors(notification_trigger, error_info)
        end
      end
    end
  end
end
