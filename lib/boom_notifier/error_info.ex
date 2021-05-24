defmodule ErrorInfo do
  @moduledoc false

  # The ErrorInfo struct holds all the information about the exception.
  # It includes the error message, the stacktrace and context information
  # (information about the request, the current controller and action,
  # among other things).

  @enforce_keys [:reason, :stack, :timestamp]
  defstruct [:name, :reason, :stack, :controller, :action, :request, :timestamp]

  @spec build(
          %{
            required(:reason) => any(),
            required(:stack) => Exception.stacktrace(),
            optional(any()) => any()
          },
          map()
        ) :: {atom(), %ErrorInfo{}}
  def build(%{reason: reason, stack: stack} = error, conn) do
    {error_reason, error_name} = error_reason(reason)

    error_info = %ErrorInfo{
      reason: error_reason,
      stack: stack,
      controller: get_in(conn.private, [:phoenix_controller]),
      action: get_in(conn.private, [:phoenix_action]),
      request: build_request_info(conn),
      timestamp: DateTime.utc_now(),
      name: error_name,
    }

    {error_type(error), error_info}
  end

  defp error_reason(%name{message: reason}), do: {reason, name}
  defp error_reason(%{message: reason}), do: {reason, "Error"}
  defp error_reason(reason) when is_binary(reason), do: error_reason(%{message: reason})
  defp error_reason(reason), do: error_reason(%{message: inspect(reason)})

  defp error_type(%{reason: %name{}}), do: name
  defp error_type(%{error: %{kind: kind}}), do: kind
  defp error_type(_), do: :error

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