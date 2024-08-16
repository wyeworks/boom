defmodule BoomNotifier.Case do
  use ExUnit.CaseTemplate

  setup do
    BoomNotifier.TestMessageProxy.subscribe(self())
  end
end
