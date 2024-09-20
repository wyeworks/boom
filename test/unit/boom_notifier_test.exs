defmodule BoomNotifier.BoomNotifierTest do
  use BoomNotifier.Case

  @receive_timeout 100

  defmodule FakeNotifier do
    def notify(_, _), do: nil
  end

  describe "to_config/1" do
    defmodule ToConfigEndpoint do
      def call(conn, _opts), do: conn

      use BoomNotifier,
        notifier: FakeNotifier
    end

    test "accepts a module name" do
      assert BoomNotifier.to_config(ToConfigEndpoint) == ToConfigEndpoint.boom_config()
    end

    test "it calls boom_notifier when a module is specified" do
      assert_raise(UndefinedFunctionError, fn ->
        BoomNotifier.to_config(Kernel)
      end)
    end

    test "accepts a keyword list" do
      assert BoomNotifier.to_config(notifier: FakeNotifier) == ToConfigEndpoint.boom_config()
    end
  end
end
