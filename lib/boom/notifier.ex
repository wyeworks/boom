defmodule Boom.Notifier do
  @callback notify(map(), keyword(String.t())) :: no_return()
end
