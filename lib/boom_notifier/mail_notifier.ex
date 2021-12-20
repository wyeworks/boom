defmodule BoomNotifier.MailNotifier do
  @moduledoc false

  @doc """
  Checks given mail notifier config contains all required keys.
  """
  @spec validate_config(keyword(String.t())) :: :ok | {:error, String.t()}
  def validate_config(options) do
    with :ok <- validate_required_config_keys(options),
         :ok <- validate_config_values(options) do
      :ok
    end
  end

  @doc """
  Creates mail subject line from a subject prefix and error reason message.
  """
  @spec build_subject(String.t(), list(%ErrorInfo{}), non_neg_integer()) :: String.t()
  def build_subject(prefix, [%ErrorInfo{reason: reason} | _], max_length) do
    String.slice("#{prefix}: #{reason}", 0..(max_length - 1))
  end

  defp validate_required_config_keys(options) do
    missing_keys = Enum.reject([:mailer, :from, :to, :subject], &Keyword.has_key?(options, &1))

    case missing_keys do
      [] -> :ok
      [missing_key] -> {:error, "#{inspect(missing_key)} parameter is missing"}
      _ -> {:error, "The following parameters are missing: #{inspect(missing_keys)}"}
    end
  end

  defp validate_config_values([{:max_subject_length, val} | rest])
       when is_integer(val) and val > 0 do
    validate_config_values(rest)
  end

  defp validate_config_values([{:max_subject_length, val} | _rest]) do
    {:error, ":max_subject_length must be non-negative integer, got #{inspect(val)}"}
  end

  defp validate_config_values([{_, _} | rest]) do
    # unknown key, dont attempt to validate
    validate_config_values(rest)
  end

  defp validate_config_values([]) do
    # nothing else to validate
    :ok
  end
end
