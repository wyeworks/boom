defmodule BoomNotifier.Config do
  @moduledoc """
  This module provides the functionality for fetching configuration settings and their defaults.
  """

  @custom_data_default :nothing
  @ignore_exceptions_default []
  @notifiers_default []
  @notification_trigger_default :always

  def custom_data do
    get_config(:custom_data, @custom_data_default)
  end

  def ignore_exceptions do
    get_config(:ignore_exceptions, @ignore_exceptions_default)
  end

  def notifiers do
    get_config(:notifiers, @notifiers_default)
  end

  def notification_trigger do
    get_config(:notification_trigger, @notification_trigger_default)
  end

  defp get_config(key, default) do
    case Application.fetch_env(:boom_notifier, key) do
      {:ok, value} -> value
      _ -> default
    end
  end
end
