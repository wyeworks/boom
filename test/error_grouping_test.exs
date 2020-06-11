defmodule ErrorGroupingTest do
  use ExUnit.Case, async: true

  @error_info "Some error information"
  @name :error_name

  setup_all do
    Boom.ErrorGrouping.start_link()
    :ok
  end

  setup do
    Agent.update(:boom, fn _ -> [] end)
  end

  test "stores the error info" do
    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert occurrences == [@error_info]

    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert occurrences == [@error_info, @error_info]
  end

  test "updates the counter when the errors are cleared" do
    {counter, _occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert counter == 1

    {counter, _occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert counter == 1

    Boom.ErrorGrouping.clear_errors(@name)
    {counter, _occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert counter == 2

    {counter, _occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert counter == 2

    Boom.ErrorGrouping.clear_errors(@name)
    {counter, _occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert counter == 4

    Boom.ErrorGrouping.clear_errors(@name)
    {counter, _occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert counter == 8
  end

  test "flushes the error list when the errors are cleared" do
    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert length(occurrences) == 1

    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert length(occurrences) == 2

    Boom.ErrorGrouping.clear_errors(@name)
    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert length(occurrences) == 1

    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert length(occurrences) == 2

    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert length(occurrences) == 3

    Boom.ErrorGrouping.clear_errors(@name)
    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert length(occurrences) == 1
  end

  test "keeps track of different error types" do
    Boom.ErrorGrouping.update_errors(@name, @error_info)
    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert occurrences == [@error_info, @error_info]

    {_counter, occurrences} =
      Boom.ErrorGrouping.update_errors(:another_error, "Another error info")

    assert occurrences == ["Another error info"]

    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert occurrences == [@error_info, @error_info, @error_info]
  end

  test "updates the counter for the proper error type" do
    Boom.ErrorGrouping.update_errors(@name, @error_info)
    {counter, _occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert counter == 1

    Boom.ErrorGrouping.clear_errors(@name)
    {counter, _occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert counter == 2

    {counter, _occurrences} =
      Boom.ErrorGrouping.update_errors(:another_error, "Another error info")

    assert counter == 1

    Boom.ErrorGrouping.clear_errors(:another_error)

    {counter, _occurrences} =
      Boom.ErrorGrouping.update_errors(:another_error, "Another error info")

    assert counter == 2

    Boom.ErrorGrouping.clear_errors(@name)
    {counter, _occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert counter == 4
  end

  test "flushes the error list for the proper error type" do
    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert length(occurrences) == 1

    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert length(occurrences) == 2

    {_counter, occurrences} =
      Boom.ErrorGrouping.update_errors(:another_error, "Another error info")

    assert length(occurrences) == 1

    Boom.ErrorGrouping.clear_errors(@name)
    {_counter, occurrences} = Boom.ErrorGrouping.update_errors(@name, @error_info)
    assert length(occurrences) == 1

    {_counter, occurrences} =
      Boom.ErrorGrouping.update_errors(:another_error, "Another error info")

    assert length(occurrences) == 2
  end
end
