defmodule BoomNotifier.TestMessageProxy do
  @moduledoc """
  A gen server that forwards messages to subscribed pids.
  Usefull to send responses to test example processes.

  Example:

  use BoomNotifier.Case

  test "receive a message from elsewhere" do
    spawn_link(fn -> send(Process.whereis(BoomNotifier.TestMessageProxy), :message) end)

    assert_receive(:message)
  end
  """
  use GenServer

  def subscribe(pid) do
    GenServer.call(__MODULE__, {:subscribe, pid}, 100)
  end

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  def stop(reason \\ :shutdown) do
    GenServer.stop(__MODULE__, reason)
  end

  @impl true
  def init(_) do
    {:ok, []}
  end

  @impl true
  def handle_call({:subscribe, pid}, _from, state) do
    Process.monitor(pid)

    {:reply, :ok, [pid | state] |> Enum.uniq()}
  end

  @impl true
  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    {:noreply, Enum.reject(state, &(&1 == pid))}
  end

  def handle_info(message, state) do
    broadcast(message, state)

    {:noreply, state}
  end

  defp broadcast(_message, []), do: nil

  defp broadcast(message, [pid | rest]) do
    send(pid, message)
    broadcast(message, rest)
  end
end
