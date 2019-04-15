defmodule ErrorInfo do
  @enforce_keys [:reason, :stack]
  defstruct [:reason, :stack, :controller]

  def build(%{message: reason}, stack, conn) do
    %ErrorInfo{
      reason: reason,
      stack: stack,
      controller: get_in(conn.private, [:phoenix_controller])
    }
  end

  def build(reason, stack, conn) when is_binary(reason) do
    build(%{message: reason}, stack, conn)
  end

  def build(reason, stack, conn) do
    build(%{message: inspect(reason)}, stack, conn)
  end
end
