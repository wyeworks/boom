defmodule BoomNotifier.Api do
  require Logger

  alias BoomNotifier.ErrorInfo
  alias BoomNotifier.ErrorStorage
  alias BoomNotifier.NotifierSenderServer

  def notify_error(settings, conn, %{kind: :error, reason: %mod{}} = error) do
    ignored_exceptions = Keyword.get(settings, :ignore_exceptions, [])

    unless Enum.member?(ignored_exceptions, mod) do
      dispatch_notify_error(settings, conn, error)
    end
  end

  def notify_error(settings, conn, error),
    do: dispatch_notify_error(settings, conn, error)

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

  defp run_callback(settings, callback) do
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

  defp dispatch_notify_error(settings, conn, error) do
    custom_data = Keyword.get(settings, :custom_data, :nothing)
    error_info = ErrorInfo.build(error, conn, custom_data)

    ErrorStorage.store_error(error_info)

    NotifierSenderServer.maybe_notify(settings, error_info)
  end
end
