defmodule Boom.Notifier do
  @callback notify(%ErrorInfo{}, keyword(String.t())) :: no_return()
end
