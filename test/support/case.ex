defmodule BoomNotifier.Case do
  @moduledoc false

  use ExUnit.CaseTemplate

  setup do
    BoomNotifier.TestMessageProxy.subscribe(self())
  end

  setup_all do
    on_exit(fn ->
      TestUtils.cancel_notification_sender_timers()
    end)
  end
end
