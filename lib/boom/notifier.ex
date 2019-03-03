defmodule Boom.Notifier do
  @callback create_payload(any(), any(), any()) :: any()
  @callback notify(any()) :: any()
end
