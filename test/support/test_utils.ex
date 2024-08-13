defmodule TestUtils do
  @moduledoc """
  Utility functions for testing.
  """

  def above_version?(boundary) do
    Version.compare(System.version(), boundary) == :gt
  end

  def flush_messages(timeout \\ 10) do
    receive do
      _message -> flush_messages()
    after
      timeout -> nil
    end
  end
end
