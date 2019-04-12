defmodule Boom.Notifier do
  @callback notify(String.t(), [String.t()], keyword(String.t())) :: no_return()
end
