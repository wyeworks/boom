defmodule Boom.Notifier do
  @callback notify(any(), any(), any()) :: any()
end
