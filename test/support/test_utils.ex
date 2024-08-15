defmodule TestUtils do
  @moduledoc """
  Utility functions for testing.
  """

  def above_version?(boundary) do
    Version.compare(System.version(), boundary) == :gt
  end

  def force_register_pid(pid, name) do
    unregister_pid(name)
    Process.register(pid, name)
  end

  def unregister_pid(name) do
    if Process.whereis(name),
      do: Process.unregister(name)
  end
end
