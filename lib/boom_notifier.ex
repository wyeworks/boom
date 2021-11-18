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
        Logger.error("Settings error: #{inspect(missing_key)} parameter missing")

      _ ->
        Logger.error(
          "Settings error: The following parameters are missing: #{inspect(missing_keys)}"
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

  defmacro __using__(_opts) do
    quote do
      @before_compile BoomNotifier
    end
  end

  defmacro __before_compile__(_) do
    quote do
      defoverridable call: 2

      import BoomNotifier

      # Notifiers validation
      walkthrough_notifiers(fn notifier, options -> validate_notifiers(notifier, options) end)

      def call(conn, opts) do
        try do
          super(conn, opts)
        catch
          kind, reason ->
            error = Exception.normalize(kind, reason, __STACKTRACE__)

            case error do
              %{kind: :error, reason: %mod{}} ->
                ignored_exceptions = Config.ignore_exceptions()

                unless Enum.member?(ignored_exceptions, mod) do
                  do_handle_errors(conn, error)
                end

              _ ->
                do_handle_errors(conn, error)
            end
        end
      end

      defp do_handle_errors(conn, error) do
        custom_data = Config.custom_data()
        {error_kind, error_info} = ErrorInfo.build(error, conn, custom_data)

        ErrorStorage.add_errors(error_kind, error_info)

        if ErrorStorage.send_notification?(error_kind) do
          occurrences = ErrorStorage.get_errors(error_kind)

          # Triggers the notification in each notifier
          walkthrough_notifiers(fn notifier, options ->
            NotifierSenderServer.send(notifier, occurrences, options)
          end)

          notification_trigger = Config.notification_trigger()
          ErrorStorage.clear_errors(notification_trigger, error_kind)

          :erlang.raise(error.kind, error.reason, error.stack)
        end
      end
    end
  end
end
