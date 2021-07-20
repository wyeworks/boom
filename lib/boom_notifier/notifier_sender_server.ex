defmodule BoomNotifier.NotifierSenderServer do
  @moduledoc false

  # This GenServer is responsible for sending the notifications in background.

  require Logger
  use GenServer

  # Client API

  def start_link(opts) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end

  def send(notifier, occurrences, options) do
    GenServer.cast(:boom_notifier_sender, {:notify, notifier, occurrences, options})
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    Process.flag(:trap_exit, true)
    {:ok, nil}
  end

  @impl true
  def handle_cast({:notify, notifier, occurrences, options}, _state) do
    spawn_link(fn ->
      notifier.notify(occurrences, options)
    end)

    {:noreply, nil}
  end

  @impl true
  def handle_info({:EXIT, _pid, :normal}, _state), do: {:noreply, nil}

  def handle_info({:EXIT, _pid, {reason, stacktrace}}, _state) do
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

    {:noreply, nil}
  end
end
