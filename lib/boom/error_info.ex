defmodule ErrorInfo do
  @enforce_keys [:reason, :stack]
  defstruct [:name, :reason, :stack, :controller, :action]

  def build(%name{message: reason}, stack, conn) do
    %{build_without_name(reason, stack, conn) | name: name}
  end

  def build(%{message: reason}, stack, conn) do
    %{build_without_name(reason, stack, conn) | name: "Error"}
  end

  def build(reason, stack, conn) when is_binary(reason) do
    build(%{message: reason}, stack, conn)
  end

  def build(reason, stack, conn) do
    build(%{message: inspect(reason)}, stack, conn)
  end

  defp build_without_name(reason, stack, conn) do
    %ErrorInfo{
      reason: reason,
      stack: stack,
      controller: get_in(conn.private, [:phoenix_controller]),
      action: get_in(conn.private, [:phoenix_action])
    }
  end
end
