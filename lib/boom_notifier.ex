defmodule BoomNotifier do
  @moduledoc false

  # Responsible for sending a notification to each notifier every time an
  # exception is raised.

  alias BoomNotifier.Config
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

  def walkthrough_notifiers(callback) do
    case Config.notifiers() do
      [] ->
        run_callback(Config.single_notifier_config(), callback)

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

  defmacro __using__(_config) do
    quote location: :keep do
      import BoomNotifier

      error_handler_in_use = {:handle_errors, 2} in Module.definitions_in(__MODULE__)
      is_router = __MODULE__ |> to_string() |> String.split(".") |> List.last() == "Router"

      if is_router and not error_handler_in_use do
        use Plug.ErrorHandler

        @impl Plug.ErrorHandler
        def handle_errors(conn, error) do
          notify_error(conn, error)
        end
      end

      # Notifiers validation
      walkthrough_notifiers(fn notifier, options -> validate_notifiers(notifier, options) end)

      def notify_error(conn, %{kind: :error, reason: %mod{}} = error) do
        unless Enum.member?(Config.ignore_exceptions(), mod) do
          do_notify_error(conn, error)
        end
      end

      def notify_error(conn, error) do
        do_notify_error(conn, error)
      end

      def manual_notify_error(conn, error) do
        {_error_kind, error_info} =
          ErrorInfo.build(%{kind: :error, reason: error, stack: []}, conn, Config.custom_data())

        # Triggers the notification in each notifier
        walkthrough_notifiers(fn notifier, options ->
          NotifierSenderServer.send(
            notifier,
            [error_info],
            options
          )
        end)
      end

      defp do_notify_error(conn, error) do
        {error_kind, error_info} = ErrorInfo.build(error, conn, Config.custom_data())

        ErrorStorage.add_errors(error_kind, error_info)

        if ErrorStorage.send_notification?(error_kind) do
          occurrences = ErrorStorage.get_errors(error_kind)

          # Triggers the notification in each notifier
          walkthrough_notifiers(fn notifier, options ->
            NotifierSenderServer.send(notifier, occurrences, options)
          end)

          notification_trigger = Config.notification_trigger()

          ErrorStorage.clear_errors(notification_trigger, error_kind)
        end
      end
    end
  end
end
