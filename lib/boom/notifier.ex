defmodule Boom.Notifier do
  @moduledoc """
  Defines a callback to be used by custom notifiers
  """

  @callback notify(%ErrorInfo{}, keyword(String.t())) :: no_return()
end
