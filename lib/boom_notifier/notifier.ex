defmodule BoomNotifier.Notifier do
  @moduledoc """
  Defines a callback to be used by custom notifiers
  """

  @callback notify(list(%ErrorInfo{}), keyword(String.t())) :: no_return()
  @callback validate_config(keyword(String.t())) :: :ok | {:error, String.t()}
  @optional_callbacks validate_config: 1
end
