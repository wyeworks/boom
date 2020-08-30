defmodule ErrorStorageTest do
  use ExUnit.Case, async: true

  @error_info "Some error information"
  @error_kind :error_kind

  setup_all do
    Boom.ErrorStorage.start_link()
    :ok
  end

  setup do
    Agent.update(:boom, fn _ -> %{} end)
  end

  describe "add_errors/2" do
    test "appends the error to its proper error kind" do
      Boom.ErrorStorage.add_errors(@error_kind, @error_info)
      assert %{@error_kind => {1, [@error_info]}} == Agent.get(:boom, fn state -> state end)

      Boom.ErrorStorage.add_errors(@error_kind, @error_info)

      assert %{@error_kind => {1, [@error_info, @error_info]}} ==
               Agent.get(:boom, fn state -> state end)

      Boom.ErrorStorage.add_errors(:another_error_kind, "Another error information")

      assert %{
        @error_kind => {1, [@error_info, @error_info]},
        :another_error_kind => {1, ["Another error information"]}
      }
    end
  end

  describe "get_errors/1" do
    test "returns the errors for the proper error kind" do
      Agent.update(:boom, fn _ ->
        %{
          @error_kind => {1, [@error_info, @error_info]},
          :another_error_kind => {1, ["another_error"]}
        }
      end)

      assert [@error_info, @error_info] == Boom.ErrorStorage.get_errors(@error_kind)
      assert ["another_error"] == Boom.ErrorStorage.get_errors(:another_error_kind)
    end

    test "returns nil if error kind does not exist" do
      assert nil == Boom.ErrorStorage.get_errors(:wrong_error_kind)
    end
  end

  describe "send_notification?/1" do
    test "returns false when count is smaller than the error length" do
      Agent.update(:boom, fn _ -> %{@error_kind => {2, [@error_info]}} end)
      assert false == Boom.ErrorStorage.send_notification?(@error_kind)
    end

    test "returns true when error length is greater or equal than count" do
      Agent.update(:boom, fn _ -> %{@error_kind => {2, [@error_info, @error_info]}} end)
      assert true == Boom.ErrorStorage.send_notification?(@error_kind)
    end

    test "returns false when error kind does not exist" do
      assert false == Boom.ErrorStorage.send_notification?(:wrong_error_kind)
    end
  end

  describe "clear_errors/2" do
    test "flushes error list" do
      Agent.update(:boom, fn _ -> %{@error_kind => {2, [@error_info, @error_info]}} end)
      Boom.ErrorStorage.clear_errors(:exponential, @error_kind)

      {_count, errors} = Agent.get(:boom, fn state -> state end) |> Map.get(@error_kind)
      assert errors == []

      Agent.update(:boom, fn _ -> %{@error_kind => {2, [@error_info, @error_info]}} end)
      Boom.ErrorStorage.clear_errors(:always, @error_kind)

      {_count, errors} = Agent.get(:boom, fn state -> state end) |> Map.get(@error_kind)
      assert errors == []
    end

    test "increases the counter when notification trigger is :exponential" do
      Agent.update(:boom, fn _ -> %{@error_kind => {1, []}} end)

      Boom.ErrorStorage.clear_errors(:exponential, @error_kind)
      {counter, _errors} = Agent.get(:boom, fn state -> state end) |> Map.get(@error_kind)
      assert counter === 2

      Boom.ErrorStorage.clear_errors(:exponential, @error_kind)
      {counter, _errors} = Agent.get(:boom, fn state -> state end) |> Map.get(@error_kind)
      assert counter === 4

      Boom.ErrorStorage.clear_errors(:exponential, @error_kind)
      {counter, _errors} = Agent.get(:boom, fn state -> state end) |> Map.get(@error_kind)
      assert counter === 8
    end

    test "does not increase the counter when notification_trigger is :always" do
      Agent.update(:boom, fn _ -> %{@error_kind => {1, []}} end)
      Boom.ErrorStorage.clear_errors(:always, @error_kind)

      {counter, _errors} = Agent.get(:boom, fn state -> state end) |> Map.get(@error_kind)
      assert counter === 1

      Boom.ErrorStorage.clear_errors(:always, @error_kind)
      {counter, _errors} = Agent.get(:boom, fn state -> state end) |> Map.get(@error_kind)
      assert counter === 1
    end

    test "updates the proper error counter" do
      Agent.update(:boom, fn _ ->
        %{@error_kind => {1, ["error1", "error2"]}, :another_error_kind => {1, ["another_error"]}}
      end)

      Boom.ErrorStorage.clear_errors(:exponential, @error_kind)
      {counter, errors} = Agent.get(:boom, fn state -> state end) |> Map.get(@error_kind)
      assert counter == 2
      assert errors == []

      {counter, errors} = Agent.get(:boom, fn state -> state end) |> Map.get(:another_error_kind)
      assert counter == 1
      assert errors == ["another_error"]
    end
  end
end
