defmodule BoomNotifier.NotificationSenderTest do
  use BoomNotifier.Case

  import TestUtils

  alias BoomNotifier.{
    ErrorInfo,
    ErrorStorage,
    NotificationSender
  }

  @time_limit 500
  @receive_timeout 100

  @settings_basic [
    notifier: __MODULE__.NotificationSenderTestNotifier,
    options: [pid_name: BoomNotifier.TestMessageProxy]
  ]

  @settings_groupping @settings_basic ++
                        [
                          time_limit: @time_limit,
                          notification_trigger: :exponential
                        ]

  defmodule NotificationSenderTestNotifier do
    def notify(error_info, opts) do
      pid = Process.whereis(opts[:pid_name])

      if pid, do: send(pid, {:notify_called, error_info})
    end
  end

  def build_error_info(_) do
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

  setup do
    clear_error_storage()

    on_exit(&flush_messages/0)
    on_exit(&cancel_notification_sender_timers/0)

    :ok
  end

  setup :build_error_info

  describe "with default notification trigger (always)" do
    test "sends a notification", %{error_info: error_info} do
      NotificationSender.trigger_notify(@settings_basic, error_info)

      {_, rcv_error_info} = assert_receive({:notify_called, _}, @receive_timeout)
      assert Map.delete(rcv_error_info, :occurrences) == Map.delete(error_info, :occurrences)
    end
  end

  describe "sync call with exponential notification trigger" do
    test "sends a notification", %{error_info: error_info} do
      NotificationSender.trigger_notify(@settings_groupping, error_info)

      {_, rcv_error_info} = assert_receive({:notify_called, _}, @receive_timeout)
      assert Map.delete(rcv_error_info, :occurrences) == Map.delete(error_info, :occurrences)
    end

    test "does not send a second notification", %{error_info: error_info} do
      ErrorStorage.store_error(error_info)
      ErrorStorage.reset_stats(error_info, :exponential)

      trigger_notify_resp = NotificationSender.trigger_notify(@settings_groupping, error_info)

      refute_receive({:notify_called, _}, @receive_timeout)
      assert {:schedule, @time_limit} = trigger_notify_resp
    end

    test "sends notification occurrences along error info", %{error_info: error_info} do
      for _ <- 1..7 do
        NotificationSender.trigger_notify(@settings_groupping, error_info)
      end

      assert_receive({:notify_called, %{occurrences: %{accumulated_occurrences: 1}}})
      assert_receive({:notify_called, %{occurrences: %{accumulated_occurrences: 2}}})
      assert_receive({:notify_called, %{occurrences: %{accumulated_occurrences: 4}}})
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
      ErrorStorage.reset_stats(error_info, :exponential)

      :ok
    end

    test "sends a second notification after a timeout", %{error_info: error_info} do
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)

      assert_receive({:notify_called, _}, @time_limit + @receive_timeout)

      assert error_info |> ErrorStorage.get_stats() |> Map.get(:accumulated_occurrences) ==
               0
    end

    test "does not send a second notification before a timeout", %{error_info: error_info} do
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)

      refute_receive({:notify_called, _}, @time_limit - 50)

      assert ErrorStorage.get_stats(error_info) |> Map.get(:accumulated_occurrences) > 0
    end

    test(
      "it does not sends a scheduled notification if another error happens",
      %{error_info: error_info}
    ) do
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)
      NotificationSender.async_trigger_notify(@settings_groupping, error_info)

      notification_sender_state =
        NotificationSender
        |> Process.whereis()
        |> :sys.get_state()

      error_key = error_info.key

      assert notification_sender_state |> Map.keys() |> length() == 1
      assert %{^error_key => _} = notification_sender_state
    end

    test(
      "it does not schedule a notification if time_limit is not specified",
      %{error_info: error_info}
    ) do
      settings = Keyword.delete(@settings_groupping, :time_limit)

      NotificationSender.async_trigger_notify(settings, error_info)
      NotificationSender.async_trigger_notify(settings, error_info)
      NotificationSender.async_trigger_notify(settings, error_info)

      notification_sender_state = :sys.get_state(Process.whereis(NotificationSender))

      assert notification_sender_state |> Map.keys() |> length() == 0
    end
  end
end
