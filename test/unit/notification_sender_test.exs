defmodule BoomNotifier.NotificationSenderTest do
  use ExUnit.Case, async: true

  import TestUtils

  alias BoomNotifier.{
    ErrorInfo,
    ErrorStorage,
    NotificationSender
  }

  @backoff_timeout 500
  @pid_name __MODULE__
  @receive_timeout 100

  @settings_basic [
    notifier: __MODULE__.NotificationSenderTestNotifier,
    options: [pid_name: @pid_name]
  ]

  @settings_groupping @settings_basic ++
                        [
                          backoff_timeout: @backoff_timeout,
                          notification_trigger: :exponential
                        ]

  defmodule NotificationSenderTestNotifier do
    def notify(error_info, opts) do
      pid = opts[:pid_name] |> Process.whereis()

      if pid, do: send(pid, {:notify_called, error_info})
    end
  end

  def build_error_info(_) do
    try do
      raise "an error"
    rescue
      e ->
        error_info =
          ErrorInfo.build(
            %{reason: e, stack: __STACKTRACE__},
            Plug.Test.conn(:get, "/"),
            :nothing
          )

        %{error_info: error_info}
    end
  end

  setup do
    self() |> Process.register(@pid_name)

    on_exit(fn ->
      Agent.update(:boom_notifier, fn _ -> %{} end)
    end)

    on_exit(&flush_messages/0)

    :ok
  end

  setup :build_error_info

  describe "with default notification trigger (always)" do
    test "sends a notification", %{error_info: error_info} do
      NotificationSender.trigger_notify(@settings_basic, error_info)

      assert_receive({:notify_called, _}, @receive_timeout)
    end
  end

  describe "sync call with exponential notification trigger" do
    test "sends a notification", %{error_info: error_info} do
      NotificationSender.trigger_notify(@settings_groupping, error_info)

      assert_receive({:notify_called, _}, @receive_timeout)
    end

    test "does not send a second notification", %{error_info: error_info} do
      ErrorStorage.store_error(error_info)
      ErrorStorage.reset_accumulated_errors(:exponential, error_info)

      trigger_notify_resp = NotificationSender.trigger_notify(@settings_groupping, error_info)

      refute_receive({:notify_called, _}, @receive_timeout)
      assert {:schedule, @backoff_timeout} = trigger_notify_resp
    end
  end

  describe "async call with exponential notification trigger" do
    test "sends a notification", %{error_info: error_info} do
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)

      assert_receive({:notify_called, _}, @receive_timeout)
    end
  end

  describe "repeated async call with exponential notification trigger" do
    setup(%{error_info: error_info}) do
      ErrorStorage.store_error(error_info)
      ErrorStorage.reset_accumulated_errors(:exponential, error_info)
    end

    test "sends a second notification after a timeout", %{error_info: error_info} do
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)

      assert_receive({:notify_called, _}, @backoff_timeout + @receive_timeout)
    end

    test "does not send a second notification before a timeout", %{error_info: error_info} do
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)

      refute_receive({:notify_called, _}, @backoff_timeout - 50)
    end

    test(
      "it does not sends a scheduled notification if another error happens",
      %{error_info: error_info}
    ) do
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)

      notification_sender_state = :sys.get_state(Process.whereis(NotificationSender))

      assert notification_sender_state |> Map.keys() |> length() == 1
    end

    test(
      "it does not schedule a notification if backoff_timeout is not specified",
      %{error_info: error_info}
    ) do
      settings = Keyword.delete(@settings_groupping, :backoff_timeout)

      NotificationSender.async_trigger_notify(settings, error_info)
      NotificationSender.async_trigger_notify(settings, error_info)
      NotificationSender.async_trigger_notify(settings, error_info)

      notification_sender_state = :sys.get_state(Process.whereis(NotificationSender))

      assert notification_sender_state |> Map.keys() |> length() == 0
    end
  end
end
