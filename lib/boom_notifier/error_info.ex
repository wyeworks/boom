defmodule BoomNotifier.ErrorInfo do
  @moduledoc false

  # The ErrorInfo struct holds all the information about the exception.
  # It includes the error message, the stacktrace, context information
  # (information about the request, the current controller and action,
  # among other things) and custom data depending on the configuration.

  @enforce_keys [:reason, :stack, :timestamp]
  defstruct [
    :key,
    :name,
    :reason,
    :stack,
    :controller,
    :action,
    :request,
    :timestamp,
    :metadata,
    :occurrences
  ]

  @type t :: %__MODULE__{}

  @type option ::
          :logger
          | [logger: [fields: list(atom())]]
          | :assigns
          | [assigns: [fields: list(atom())]]
  @type custom_data_strategy_type :: :nothing | option | [option]

  @spec build(
          %{
            required(:reason) => any(),
            required(:stack) => Exception.stacktrace(),
            optional(any()) => any()
          },
          map(),
          custom_data_strategy_type
        ) :: __MODULE__.t()
  def build(%{reason: reason, stack: stack}, conn, custom_data_strategy) do
    {error_reason, error_name} = error_reason(reason)

    %__MODULE__{
      reason: error_reason,
      stack: stack,
      controller: get_in(conn.private, [:phoenix_controller]),
      action: get_in(conn.private, [:phoenix_action]),
      request: build_request_info(conn),
      timestamp: DateTime.utc_now(),
      name: error_name,
      metadata: build_custom_data(conn, custom_data_strategy)
    }
    |> ensure_key()
  end

  defp error_reason(%name{message: reason}), do: {reason, name}
  defp error_reason(%{message: reason}), do: {reason, "Error"}
  defp error_reason(reason) when is_binary(reason), do: error_reason(%{message: reason})
  defp error_reason(reason), do: error_reason(%{message: inspect(reason)})

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

  @spec build_custom_data(map(), custom_data_strategy_type) :: map()
  defp build_custom_data(_conn, :nothing), do: nil

  defp build_custom_data(_conn, :logger),
    do: %{logger: Enum.into(Logger.metadata(), %{})}

  defp build_custom_data(_conn, logger: [fields: field_names]),
    do: %{
      logger:
        Enum.reduce(field_names, %{}, fn field_name, acc ->
          Map.put(acc, field_name, Logger.metadata()[field_name])
        end)
    }

  defp build_custom_data(conn, :assigns),
    do: %{assigns: conn.assigns}

  defp build_custom_data(conn, assigns: [fields: field_names]),
    do: %{
      assigns:
        Enum.reduce(field_names, %{}, fn field_name, acc ->
          Map.put(acc, field_name, conn.assigns[field_name])
        end)
    }

  defp build_custom_data(conn, options),
    do:
      Enum.reduce(options, %{}, fn opt, acc ->
        Map.merge(acc, build_custom_data(conn, opt))
      end)

  # Generates a unique hash key based on the error info. The timestamp and the
  # request info is removed so we don't get different keys for the same error.
  #
  # The map is converted to a string using `inspect()` so we can hash it using
  # the crc32 algorithm that was taken from the Exception Notification library
  # for Rails
  @spec generate_error_key(__MODULE__.t()) :: non_neg_integer()
  def generate_error_key(error_info) do
    error_info
    |> Map.delete(:request)
    |> Map.delete(:metadata)
    |> Map.delete(:timestamp)
    |> Map.update(:stack, nil, fn stacktrace -> List.first(stacktrace) end)
    |> inspect()
    |> :erlang.crc32()
  end

  def ensure_key(%__MODULE__{} = error_info) do
    error_info |> Map.put_new_lazy(:key, fn -> generate_error_key(error_info) end)
  end
end
