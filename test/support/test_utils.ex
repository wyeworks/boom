defmodule TestUtils do
  @moduledoc """
  Utility functions for testing.
  """

  def above_version?(boundary) do
    Version.compare(System.version(), boundary) == :gt
  end

  def clear_error_storage do
    Agent.update(:boom_notifier, fn _ -> %{} end)
  end

  def cancel_notification_sender_timers do
    :sys.get_state(Process.whereis(BoomNotifier.NotificationSender))
    |> Map.values()
    |> Enum.each(&Process.cancel_timer/1)
  end

  def flush_messages(timeout \\ 10) do
    receive do
      _message -> flush_messages()
    after
      timeout -> nil
    end
  end
end
