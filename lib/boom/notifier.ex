defmodule Boom.Notifier do
  @moduledoc """
  Defines a callback to be used by custom notifiers
  """

  @callback notify(list(%ErrorInfo{}), keyword(String.t())) :: no_return()
end
