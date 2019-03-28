defmodule Boom.Notifier do
  @type reason :: atom() | {:message, String.t()}
  @callback notify(reason, [String.t()], keyword(String.t())) :: no_return()
end
