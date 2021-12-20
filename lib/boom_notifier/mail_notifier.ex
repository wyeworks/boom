defmodule BoomNotifier.MailNotifier do
  @moduledoc false

  @doc """
  Checks given mail notifier config contains all required keys.
  """
  @spec validate_config(keyword(String.t())) :: :ok | {:error, String.t()}
  def validate_config(options) do
    missing_keys = Enum.reject([:mailer, :from, :to, :subject], &Keyword.has_key?(options, &1))

    case missing_keys do
      [] -> :ok
      [missing_key] -> {:error, "#{inspect(missing_key)} parameter is missing"}
      _ -> {:error, "The following parameters are missing: #{inspect(missing_keys)}"}
    end
  end

  @doc """
  Creates mail subject line from a subject prefix and error reason message.
  """
  @spec build_subject(String.t(), list(%ErrorInfo{}), non_neg_integer()) :: String.t()
  def build_subject(prefix, [%ErrorInfo{reason: reason} | _], max_length) do
    String.slice("#{prefix}: #{reason}", 0..(max_length - 1))
  end
end
