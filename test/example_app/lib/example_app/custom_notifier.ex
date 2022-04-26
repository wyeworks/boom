defmodule ExampleApp.MemoryErrorStorage do
  @moduledoc """
  Store in an Agent the last error
  """

  use Agent

  def start_link(_opts), do: Agent.start_link(fn -> nil end, name: :memory_notifier)
  def get_error(), do: Agent.get(:memory_notifier, fn state -> state end)
  def replace_error(error), do: Agent.update(:memory_notifier, fn _state -> error end)
end

defmodule ExampleApp.CustomNotifier do
  @behaviour BoomNotifier.Notifier

  @impl BoomNotifier.Notifier
  def notify(error, _options) do
    ExampleApp.MemoryErrorStorage.replace_error(error)
  end
end
