defmodule ErrorGroupingTest do
  use ExUnit.Case, async: true

  @error_info "Some error information"
  @error_reason :error_reason

  setup_all do
    Boom.ErrorGrouping.start_link()
    :ok
  end

  setup do
    Agent.update(:boom, fn _ -> [] end)
  end

  describe "update_errors/2" do
    test "appends the error to its proper error reason" do
      Boom.ErrorGrouping.update_errors(@error_reason, @error_info)
      assert [{@error_reason, {1, [@error_info]}}] == Agent.get(:boom, fn state -> state end)

      Boom.ErrorGrouping.update_errors(@error_reason, @error_info)

      assert [{@error_reason, {1, [@error_info, @error_info]}}] ==
               Agent.get(:boom, fn state -> state end)

      Boom.ErrorGrouping.update_errors(:another_error, @error_info)

      assert [
               {@error_reason, {1, [@error_info, @error_info]}},
               {:another_error, {1, [@error_info]}}
             ] == Agent.get(:boom, fn state -> state end)
    end
  end

  describe "get_errors/1" do
    test "returns the errors for the proper error reason" do
      Agent.update(:boom, fn _ ->
        [
          {@error_reason, {1, [@error_info, @error_info]}},
          {:another_error, {1, ["another_error"]}}
        ]
      end)

      assert [@error_info, @error_info] == Boom.ErrorGrouping.get_errors(@error_reason)
      assert ["another_error"] == Boom.ErrorGrouping.get_errors(:another_error)
    end

    test "returns nil if error reason does not exist" do
      assert nil == Boom.ErrorGrouping.get_errors(:wrong_error_reason)
    end
  end

  describe "send_notification?/1" do
    test "returns false when count is smaller than the error length" do
      Agent.update(:boom, fn _ -> [{@error_reason, {2, [@error_info]}}] end)
      assert false == Boom.ErrorGrouping.send_notification?(@error_reason)
    end

    test "returns true when error length is bigger than count" do
      Agent.update(:boom, fn _ -> [{@error_reason, {2, [@error_info, @error_info]}}] end)
      assert true == Boom.ErrorGrouping.send_notification?(@error_reason)
    end

    test "returns false when error reason does not exist" do
      assert false == Boom.ErrorGrouping.send_notification?(:wrong_error_reason)
    end
  end

  describe "clear_errors/2" do
    test "flushes error list" do
      Agent.update(:boom, fn _ -> [{@error_reason, {2, [@error_info, @error_info]}}] end)
      Boom.ErrorGrouping.clear_errors(true, @error_reason)

      {_count, errors} = Agent.get(:boom, fn state -> state end) |> Keyword.get(@error_reason)
      assert errors == []

      Agent.update(:boom, fn _ -> [{@error_reason, {2, [@error_info, @error_info]}}] end)
      Boom.ErrorGrouping.clear_errors(false, @error_reason)

      {_count, errors} = Agent.get(:boom, fn state -> state end) |> Keyword.get(@error_reason)
      assert errors == []
    end

    test "increases the counter when error_grouping is true" do
      Agent.update(:boom, fn _ -> [{@error_reason, {1, []}}] end)

      Boom.ErrorGrouping.clear_errors(true, @error_reason)
      {counter, _errors} = Agent.get(:boom, fn state -> state end) |> Keyword.get(@error_reason)
      assert counter === 2

      Boom.ErrorGrouping.clear_errors(true, @error_reason)
      {counter, _errors} = Agent.get(:boom, fn state -> state end) |> Keyword.get(@error_reason)
      assert counter === 4

      Boom.ErrorGrouping.clear_errors(true, @error_reason)
      {counter, _errors} = Agent.get(:boom, fn state -> state end) |> Keyword.get(@error_reason)
      assert counter === 8
    end

    test "does not increases the counter when error_grouping is false" do
      Agent.update(:boom, fn _ -> [{@error_reason, {1, []}}] end)
      Boom.ErrorGrouping.clear_errors(false, @error_reason)

      {counter, _errors} = Agent.get(:boom, fn state -> state end) |> Keyword.get(@error_reason)
      assert counter === 1

      Boom.ErrorGrouping.clear_errors(false, @error_reason)
      {counter, _errors} = Agent.get(:boom, fn state -> state end) |> Keyword.get(@error_reason)
      assert counter === 1
    end

    test "updates the proper error counter" do
      Agent.update(:boom, fn _ ->
        [{@error_reason, {1, ["error1", "error2"]}}, {:another_error, {1, ["another_error"]}}]
      end)

      Boom.ErrorGrouping.clear_errors(true, @error_reason)
      {counter, errors} = Agent.get(:boom, fn state -> state end) |> Keyword.get(@error_reason)
      assert counter == 2
      assert errors == []

      {counter, errors} = Agent.get(:boom, fn state -> state end) |> Keyword.get(:another_error)
      assert counter == 1
      assert errors == ["another_error"]
    end
  end
end
