defmodule BoomNotifier.NotifierSenderServer do
  @moduledoc false

  # This GenServer is responsible for sending the notifications in background.

  require Logger
  use GenServer

  alias BoomNotifier.ErrorStorage

  # Client API

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def send(notifier, occurrences, options) do
    GenServer.cast(__MODULE__, {:notify, notifier, occurrences, options})
  end

  def maybe_notify(caller_mod, error_info) do
    GenServer.cast(__MODULE__, {:maybe_notify, caller_mod, error_info})
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    Process.flag(:trap_exit, true)
    {:ok, %{}}
  end

  @impl true
  def handle_cast({:notify, notifier, occurrences, options}, state) do
    spawn_link(fn ->
      notifier.notify(occurrences, options)
    end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:maybe_notify, settings, error_info}, state) do
    notification_trigger = Keyword.get(settings, :notification_trigger, :always)

    if ErrorStorage.send_notification?(error_info) do
      occurrences = Map.put(error_info, :occurrences, ErrorStorage.get_error_stats(error_info))

      BoomNotifier.Api.walkthrough_notifiers(
        settings,
        fn notifier, options -> send(notifier, occurrences, options) end
      )

      ErrorStorage.reset_accumulated_errors(notification_trigger, error_info)
      # TODO: reset timer that might be scheduled below
      {:noreply, state}
    else
      # TODO: send_after(:timeout_notify) and store timer
      # It's already ugly to be passing around settings here,
      # how do we access settings in handle_cast({:timeout_notify, settings, error_info})?
      {:noreply, state}
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
end
