defmodule BoomNotifier.Case do
  @moduledoc false

  use ExUnit.CaseTemplate

  setup do
    BoomNotifier.TestMessageProxy.subscribe(self())
  end
end
