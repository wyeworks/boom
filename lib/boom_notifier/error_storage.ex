defmodule BoomNotifier.ErrorStorage do
  @moduledoc false

  # Keeps track of the errors grouped by type and a counter so the notifier
  # knows the next time it should be executed

  defstruct [:accumulated_occurrences, :first_occurrence, :last_occurrence]

  use Agent, start: {__MODULE__, :start_link, []}

  @spec start_link() :: Agent.on_start()
  def start_link do
    Agent.start_link(fn -> %{} end, name: :boom_notifier)
  end

  @spec add_error(%ErrorInfo{}) :: :ok
  def add_error(error_info) do
    error_hash_key = generate_error_key(error_info)
    timestamp = DateTime.utc_now()

    Agent.update(
      :boom_notifier,
      &Map.update(
        &1,
        error_hash_key,
        %{
          accumulated_occurrences: 1,
          max_storage_capacity: 1,
          first_occurrence: timestamp,
          last_occurrence: timestamp
        },
        fn error_storage_item ->
          error_storage_item
          |> Map.update!(:accumulated_occurrences, fn current -> current + 1 end)
          |> Map.update!(:first_occurrence, fn first_occurrence ->
            first_occurrence || timestamp
          end)
          |> Map.update!(:last_occurrence, fn _ -> timestamp end)
        end
      )
    )
  end

  @spec get_error_storage_item(%ErrorInfo{}) :: %__MODULE__{}
  def get_error_storage_item(error_info) do
    error_hash_key = generate_error_key(error_info)

    Agent.get(:boom_notifier, fn state -> state end)
    |> Map.get(error_hash_key)
  end

  @spec send_notification?(%ErrorInfo{}) :: boolean()
  def send_notification?(error_info) do
    error_hash_key = generate_error_key(error_info)

    error_storage_item =
      Agent.get(:boom_notifier, fn state -> state end)
      |> Map.get(error_hash_key)

    accumulated_occurrences = Map.get(error_storage_item, :accumulated_occurrences)
    max_storage_capacity = Map.get(error_storage_item, :max_storage_capacity)

    if accumulated_occurrences >= max_storage_capacity, do: true, else: false
  end

  @type error_strategy :: :always | :exponential | [exponential: [limit: non_neg_integer()]]

  @spec reset_accumulated_errors(error_strategy, %ErrorInfo{}) :: :ok
  def reset_accumulated_errors(:exponential, error_info) do
    error_hash_key = generate_error_key(error_info)

    Agent.update(
      :boom_notifier,
      &Map.update!(&1, error_hash_key, fn error_storage_item ->
        clear_values(error_storage_item)
        |> Map.update!(:max_storage_capacity, fn current -> current * 2 end)
      end)
    )
  end

  def reset_accumulated_errors([exponential: [limit: limit]], error_info) do
    error_hash_key = generate_error_key(error_info)

    Agent.update(
      :boom_notifier,
      &Map.update!(&1, error_hash_key, fn error_storage_item ->
        clear_values(error_storage_item)
        |> Map.update!(:max_storage_capacity, fn current -> min(current * 2, limit) end)
      end)
    )
  end

  def reset_accumulated_errors(:always, error_info) do
    error_hash_key = generate_error_key(error_info)

    Agent.update(
      :boom_notifier,
      &Map.update!(&1, error_hash_key, fn error_storage_item ->
        clear_values(error_storage_item)
        |> Map.replace!(:max_storage_capacity, 1)
      end)
    )
  end

  # Generates a unique hash key based on the error info. The timestamp and the
  # request info is removed so we don't get different keys for the same error.
  #
  # The map is converted to a string using `inspect()` so we can hash it using
  # the crc32 algorithm that was taken from the Exception Notification library
  # for Rails
  @spec generate_error_key(%ErrorInfo{}) :: non_neg_integer()
  defp generate_error_key(error_info) do
    error_info
    |> Map.delete(:request)
    |> Map.delete(:timestamp)
    |> inspect()
    |> :erlang.crc32()
  end

  defp clear_values(error_storage_item) do
    error_storage_item
    |> Map.replace!(:accumulated_occurrences, 0)
    |> Map.replace!(:first_occurrence, nil)
    |> Map.replace!(:last_occurrence, nil)
  end
end
