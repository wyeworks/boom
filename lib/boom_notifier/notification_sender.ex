defmodule BoomNotifier.NotificationSender do
  @moduledoc false

  # This GenServer is responsible for sending the notifications in background.

  require Logger
  use GenServer

  alias BoomNotifier.ErrorStorage

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def async_trigger_notify(settings, error_info) do
    GenServer.cast(__MODULE__, {:trigger_notify, settings, error_info})
  end

  def trigger_notify(settings, error_info) do
    ErrorStorage.accumulate(error_info)

    do_trigger_notify(
      Keyword.get(settings, :groupping, :count),
      settings,
      error_info
    )
  end

  defp do_trigger_notify(:count, settings, error_info) do
    time_limit = Keyword.get(settings, :time_limit)

    if ErrorStorage.send_notification?(error_info) do
      notify_all(settings, error_info)
      :ok
    else
      if time_limit do
        {:schedule, time_limit}
      else
        :ignored
      end
    end
  end

  defp do_trigger_notify(:time, settings, error_info) do
    throttle = Keyword.get(settings, :throttle, 100)
    time_limit = Keyword.get(settings, :time_limit)

    stats = ErrorStorage.get_stats(error_info)

    if ErrorStorage.eleapsed(stats) >= time_limit do
      notify_all(settings, error_info)
      :ok
    else
      {:schedule, throttle}
    end
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    Process.flag(:trap_exit, true)
    {:ok, %{}}
  end

  @impl true
  def handle_cast({:notify, notifier, occurrences, options}, state) do
    notify(notifier, occurrences, options)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:trigger_notify, settings, error_info}, state) do
    {timer, state} = Map.pop(state, error_info.key)

    cancel_timer(timer)

    case trigger_notify(settings, error_info) do
      :ignored ->
        {:noreply, state}

      :ok ->
        {:noreply, state}

      {:schedule, timeout} ->
        timer = Process.send_after(self(), {:notify_all, settings, error_info}, timeout)
        {:noreply, state |> Map.put(error_info.key, timer)}
    end
  end

  @impl true
  def handle_info({:EXIT, _pid, :normal}, state), do: {:noreply, state}

  def handle_info({:EXIT, _pid, {reason, stacktrace}}, state) do
    error_info = Exception.format_banner(:error, reason, stacktrace)

    failing_notifier =
      case stacktrace do
        [{module, function, arity, _} | _] ->
          Exception.format_mfa(module, function, arity)

        [first_stack_entry | _] ->
          Exception.format_stacktrace_entry(first_stack_entry)
      end

    Logger.error(
      "An error occurred when sending a notification: #{error_info} in #{failing_notifier}"
    )

    {:noreply, state}
  end

  def handle_info({:notify_all, settings, error_info}, state) do
    notify_all(settings, error_info)

    {:noreply, state |> Map.delete(error_info.key)}
  end

  # Private methods

  defp notify(notifier, occurrences, options) do
    spawn_link(fn ->
      notifier.notify(occurrences, options)
    end)
  end

  defp notify_all(settings, error_info) do
    count_strategy = Keyword.get(settings, :count)

    occurrences = Map.put(error_info, :occurrences, ErrorStorage.get_stats(error_info))
    ErrorStorage.reset_accumulated_errors(count_strategy, error_info)

    BoomNotifier.walkthrough_notifiers(
      settings,
      fn notifier, options -> notify(notifier, occurrences, options) end
    )
  end

  defp cancel_timer(nil), do: nil
  defp cancel_timer(timer), do: Process.cancel_timer(timer)
end
