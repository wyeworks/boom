defmodule ErrorStorageTest do
  use ExUnit.Case, async: true

  alias BoomNotifier.ErrorInfo
  alias BoomNotifier.ErrorStorage

  @timestamp DateTime.utc_now()

  @error_info %ErrorInfo{
    reason: "Some error information",
    stack: ["line 1"],
    timestamp: @timestamp
  }

  setup_all do
    ErrorStorage.start_link()
    :ok
  end

  setup do
    Agent.update(:boom_notifier, fn _ -> %{} end)
  end

  describe "store_error/1" do
    test "groups errors by type" do
      another_timestamp = DateTime.utc_now()

      another_error_info = %ErrorInfo{
        reason: "Another error information",
        stack: ["line 2"],
        timestamp: another_timestamp
      }

      ErrorStorage.store_error(@error_info)
      ErrorStorage.store_error(@error_info)
      ErrorStorage.store_error(another_error_info)

      [error_stat_1, error_stat_2] =
        Agent.get(:boom_notifier, fn state -> state end)
        |> Map.values()

      assert error_stat_1 == %ErrorStorage{
               __max_storage_capacity__: 1,
               accumulated_occurrences: 1,
               first_occurrence: another_timestamp,
               last_occurrence: another_timestamp
             }

      assert error_stat_2 == %ErrorStorage{
               __max_storage_capacity__: 1,
               accumulated_occurrences: 2,
               first_occurrence: @timestamp,
               last_occurrence: @timestamp
             }
    end
  end

  describe "get_error_stats/1" do
    test "returns the errors for the proper error kind" do
      ErrorStorage.store_error(@error_info)
      ErrorStorage.store_error(@error_info)

      assert ErrorStorage.get_error_stats(@error_info) ==
               %ErrorStorage{
                 __max_storage_capacity__: 1,
                 accumulated_occurrences: 2,
                 first_occurrence: @timestamp,
                 last_occurrence: @timestamp
               }

      another_timestamp = DateTime.utc_now()

      another_error_info = %ErrorInfo{
        reason: "Another error information",
        stack: ["line 2"],
        timestamp: another_timestamp
      }

      ErrorStorage.store_error(another_error_info)

      assert ErrorStorage.get_error_stats(another_error_info) ==
               %ErrorStorage{
                 __max_storage_capacity__: 1,
                 accumulated_occurrences: 1,
                 first_occurrence: another_timestamp,
                 last_occurrence: another_timestamp
               }
    end

    test "returns nil if error info does not exist" do
      assert ErrorStorage.get_error_stats(@error_info) == nil
    end
  end

  describe "send_notification?/1" do
    test "returns false when count is smaller than the error length" do
      # increase the max capacity to 2
      ErrorStorage.store_error(@error_info)
      ErrorStorage.reset_accumulated_errors(:exponential, @error_info)
      ErrorStorage.store_error(@error_info)

      refute ErrorStorage.send_notification?(@error_info)
    end

    test "returns true when error length is greater or equal than count" do
      # creates the error key
      ErrorStorage.store_error(@error_info)
      # increase the max capacity to 2
      ErrorStorage.reset_accumulated_errors(:exponential, @error_info)
      ErrorStorage.store_error(@error_info)
      ErrorStorage.store_error(@error_info)

      another_error_info = %ErrorInfo{
        reason: "Another error information",
        stack: ["line 2"],
        timestamp: @timestamp
      }

      # creates the error key
      ErrorStorage.store_error(another_error_info)
      # increase the max capacity to 2
      ErrorStorage.reset_accumulated_errors(:exponential, another_error_info)
      ErrorStorage.store_error(another_error_info)
      ErrorStorage.store_error(another_error_info)
      ErrorStorage.store_error(another_error_info)

      assert ErrorStorage.send_notification?(@error_info)
      assert ErrorStorage.send_notification?(another_error_info)
    end

    test "returns false when error kind does not exist" do
      refute ErrorStorage.send_notification?(%{})
    end
  end

  describe "reset_accumulated_errors/2" do
    test "increases the counter when notification trigger is :exponential" do
      ErrorStorage.store_error(@error_info)

      ErrorStorage.reset_accumulated_errors(:exponential, @error_info)
      [error_stat] = Agent.get(:boom_notifier, fn state -> state end) |> Map.values()

      assert error_stat == %ErrorStorage{
               __max_storage_capacity__: 2,
               accumulated_occurrences: 0,
               first_occurrence: nil,
               last_occurrence: nil
             }

      ErrorStorage.reset_accumulated_errors(:exponential, @error_info)
      [error_stat] = Agent.get(:boom_notifier, fn state -> state end) |> Map.values()

      assert error_stat == %ErrorStorage{
               __max_storage_capacity__: 4,
               accumulated_occurrences: 0,
               first_occurrence: nil,
               last_occurrence: nil
             }

      ErrorStorage.reset_accumulated_errors(:exponential, @error_info)
      [error_stat] = Agent.get(:boom_notifier, fn state -> state end) |> Map.values()

      assert error_stat == %ErrorStorage{
               __max_storage_capacity__: 8,
               accumulated_occurrences: 0,
               first_occurrence: nil,
               last_occurrence: nil
             }
    end

    test "increases the counter when notification trigger is :exponential and :limit is set" do
      ErrorStorage.store_error(@error_info)

      ErrorStorage.reset_accumulated_errors([exponential: [limit: 5]], @error_info)
      [error_stat] = Agent.get(:boom_notifier, fn state -> state end) |> Map.values()

      assert error_stat == %ErrorStorage{
               __max_storage_capacity__: 2,
               accumulated_occurrences: 0,
               first_occurrence: nil,
               last_occurrence: nil
             }

      ErrorStorage.reset_accumulated_errors([exponential: [limit: 5]], @error_info)
      [error_stat] = Agent.get(:boom_notifier, fn state -> state end) |> Map.values()

      assert error_stat == %ErrorStorage{
               __max_storage_capacity__: 4,
               accumulated_occurrences: 0,
               first_occurrence: nil,
               last_occurrence: nil
             }

      ErrorStorage.reset_accumulated_errors([exponential: [limit: 5]], @error_info)
      [error_stat] = Agent.get(:boom_notifier, fn state -> state end) |> Map.values()

      assert error_stat == %ErrorStorage{
               __max_storage_capacity__: 5,
               accumulated_occurrences: 0,
               first_occurrence: nil,
               last_occurrence: nil
             }
    end

    test "does not increase the counter when notification_trigger is :always" do
      ErrorStorage.store_error(@error_info)

      ErrorStorage.reset_accumulated_errors(:always, @error_info)
      [error_stat] = Agent.get(:boom_notifier, fn state -> state end) |> Map.values()

      assert error_stat == %ErrorStorage{
               __max_storage_capacity__: 1,
               accumulated_occurrences: 0,
               first_occurrence: nil,
               last_occurrence: nil
             }

      ErrorStorage.reset_accumulated_errors(:always, @error_info)
      [error_stat] = Agent.get(:boom_notifier, fn state -> state end) |> Map.values()

      assert error_stat == %ErrorStorage{
               __max_storage_capacity__: 1,
               accumulated_occurrences: 0,
               first_occurrence: nil,
               last_occurrence: nil
             }

      ErrorStorage.reset_accumulated_errors(:always, @error_info)
    end

    test "updates the proper error max capacity" do
      another_error_info = %ErrorInfo{
        reason: "Another error information",
        stack: ["line 2"],
        timestamp: @timestamp
      }

      ErrorStorage.store_error(@error_info)
      ErrorStorage.store_error(another_error_info)

      ErrorStorage.reset_accumulated_errors(:exponential, @error_info)

      [error_stat_1, error_stat_2] =
        Agent.get(:boom_notifier, fn state -> state end) |> Map.values()

      assert error_stat_1 == %ErrorStorage{
               __max_storage_capacity__: 1,
               accumulated_occurrences: 1,
               first_occurrence: @timestamp,
               last_occurrence: @timestamp
             }

      assert error_stat_2 == %ErrorStorage{
               __max_storage_capacity__: 2,
               accumulated_occurrences: 0,
               first_occurrence: nil,
               last_occurrence: nil
             }
    end
  end
end
