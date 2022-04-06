defmodule BoomNotifier.ErrorStorage do
  @moduledoc false

  # Keeps track of the errors grouped by type and a counter so the notifier
  # knows the next time it should be executed

  @enforce_keys [:accumulated_occurrences, :first_occurrence, :last_occurrence]
  defstruct [
    :accumulated_occurrences,
    :first_occurrence,
    :last_occurrence,
    :__max_storage_capacity__
  ]

  @type t :: %__MODULE__{}
  @type error_strategy :: :always | :exponential | [exponential: [limit: non_neg_integer()]]

  use Agent, start: {__MODULE__, :start_link, []}

  @spec start_link() :: Agent.on_start()
  def start_link do
    Agent.start_link(fn -> %{} end, name: :boom_notifier)
  end

  @doc """
  Stores information about how many times an error occurred.

  This information is stored in a map where the key is the error info hash.
  Every time this function is called with an error info, the accumulated
  occurrences is increased and it also updates the first and last time it
  happened.
  """
  @spec store_error(ErrorInfo.t()) :: :ok
  def store_error(error_info) do
    error_hash_key = generate_error_key(error_info)
    timestamp = error_info.timestamp || DateTime.utc_now()

    default_error_storage_info = %__MODULE__{
      accumulated_occurrences: 1,
      first_occurrence: timestamp,
      last_occurrence: timestamp,
      __max_storage_capacity__: 1
    }

    Agent.update(
      :boom_notifier,
      &Map.update(
        &1,
        error_hash_key,
        default_error_storage_info,
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

  @doc """
  Given an error info, it returns the aggregated info stored in the agent.
  """
  @spec get_error_stats(ErrorInfo.t()) :: %__MODULE__{}
  def get_error_stats(error_info) do
    error_hash_key = generate_error_key(error_info)

    Agent.get(:boom_notifier, fn state -> state end)
    |> Map.get(error_hash_key)
  end

  @doc """
  Tells whether a notification for an error info is ready to be sent.

  Returns true if the accumulated_occurrences is above or equal to the max
  storage capacity.
  """
  @spec send_notification?(ErrorInfo.t()) :: boolean()
  def send_notification?(error_info) do
    error_hash_key = generate_error_key(error_info)

    error_storage_item =
      Agent.get(:boom_notifier, fn state -> state end)
      |> Map.get(error_hash_key)

    do_send_notification?(error_storage_item)
  end

  @doc """
  Reset the accumulated_occurrences for the given error info to zero. It also
  increments the max storage capacity based on the notification strategy.
  """
  @spec reset_accumulated_errors(error_strategy, ErrorInfo.t()) :: :ok
  def reset_accumulated_errors(:exponential, error_info) do
    error_hash_key = generate_error_key(error_info)

    Agent.update(
      :boom_notifier,
      &Map.update!(&1, error_hash_key, fn error_storage_item ->
        clear_values(error_storage_item)
        |> Map.update!(:__max_storage_capacity__, fn current -> current * 2 end)
      end)
    )
  end

  def reset_accumulated_errors([exponential: [limit: limit]], error_info) do
    error_hash_key = generate_error_key(error_info)

    Agent.update(
      :boom_notifier,
      &Map.update!(&1, error_hash_key, fn error_storage_item ->
        clear_values(error_storage_item)
        |> Map.update!(:__max_storage_capacity__, fn current -> min(current * 2, limit) end)
      end)
    )
  end

  def reset_accumulated_errors(:always, error_info) do
    error_hash_key = generate_error_key(error_info)

    Agent.update(
      :boom_notifier,
      &Map.update!(&1, error_hash_key, fn error_storage_item ->
        clear_values(error_storage_item)
        |> Map.replace!(:__max_storage_capacity__, 1)
      end)
    )
  end

  # Generates a unique hash key based on the error info. The timestamp and the
  # request info is removed so we don't get different keys for the same error.
  #
  # The map is converted to a string using `inspect()` so we can hash it using
  # the crc32 algorithm that was taken from the Exception Notification library
  # for Rails
  @spec generate_error_key(ErrorInfo.t()) :: non_neg_integer()
  defp generate_error_key(error_info) do
    error_info
    |> Map.delete(:request)
    |> Map.delete(:metadata)
    |> Map.delete(:timestamp)
    |> Map.update(:stack, nil, fn stacktrace -> List.first(stacktrace) end)
    |> inspect()
    |> :erlang.crc32()
  end

  defp clear_values(error_storage_item) do
    error_storage_item
    |> Map.replace!(:accumulated_occurrences, 0)
    |> Map.replace!(:first_occurrence, nil)
    |> Map.replace!(:last_occurrence, nil)
  end

  @spec do_send_notification?(ErrorInfo.t() | nil) :: boolean()
  defp do_send_notification?(nil), do: false

  defp do_send_notification?(error_storage_item) do
    accumulated_occurrences = Map.get(error_storage_item, :accumulated_occurrences)
    max_storage_capacity = Map.get(error_storage_item, :__max_storage_capacity__)

    if accumulated_occurrences >= max_storage_capacity, do: true, else: false
  end
end
