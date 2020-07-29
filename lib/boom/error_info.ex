defmodule ErrorInfo do
  @moduledoc false
  # The ErrorInfo struct holds all the information about the exception.
  # It includes the error message, the stacktrace and context information
  # (information about the request, the current controller and action,
  # among other things).

  @enforce_keys [:reason, :stack]
  defstruct [:name, :reason, :stack, :controller, :action, :request]

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

  def get_reason(%{reason: %name{}}), do: name
  def get_reason(%{error: %{kind: kind}}), do: kind
  def get_reason(_), do: :error

  defp build_without_name(reason, stack, conn) do
    %ErrorInfo{
      reason: reason,
      stack: stack,
      controller: get_in(conn.private, [:phoenix_controller]),
      action: get_in(conn.private, [:phoenix_action]),
      request: build_request_info(conn)
    }
  end

  defp build_request_info(conn) do
    %{
      path: conn.request_path,
      method: conn.method,
      url: get_full_url(conn),
      port: conn.port,
      scheme: conn.scheme,
      query_string: conn.query_string,
      client_ip: format_ip(conn.remote_ip)
    }
  end

  # Credit: https://github.com/jarednorman/plugsnag/blob/master/lib/plugsnag/basic_error_report_builder.ex
  defp get_full_url(conn) do
    base = "#{conn.scheme}://#{conn.host}#{conn.request_path}"

    case conn.query_string do
      "" -> base
      qs -> "#{base}?#{qs}"
    end
  end

  # Credit: https://github.com/jarednorman/plugsnag/blob/master/lib/plugsnag/basic_error_report_builder.ex
  defp format_ip(ip) do
    ip
    |> Tuple.to_list()
    |> Enum.join(".")
  end
end
