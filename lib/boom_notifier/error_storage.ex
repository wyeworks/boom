defmodule BoomNotifier.ErrorStorage do
  @moduledoc false

  # Keeps track of the errors grouped by type and a counter so the notifier
  # knows the next time it should be executed

  alias BoomNotifier.ErrorInfo

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
    %{key: error_hash_key} = error_info
    timestamp = error_info.timestamp || DateTime.utc_now()

    Agent.update(
      :boom_notifier,
      &Map.update(
        &1,
        error_hash_key,
        default_error_storage(timestamp),
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
    %{key: error_hash_key} = error_info

    Agent.get(:boom_notifier, &Map.get(&1, error_hash_key))
  end

  @doc """
  Tells whether a notification for an error info is ready to be sent.

  Returns true if the accumulated_occurrences is above or equal to the max
  storage capacity.
  """
  @spec send_notification?(ErrorInfo.t()) :: boolean()
  def send_notification?(error_info) do
    error_info
    |> get_error_stats()
    |> do_send_notification?()
  end

  @doc """
  Reset the accumulated_occurrences for the given error info to zero. It also
  increments the max storage capacity based on the notification strategy.

  Returns error storage entry before reset
  """
  @spec reset(ErrorInfo.t()) :: %__MODULE__{}
  @spec reset(ErrorInfo.t(), count_strategy :: :exponential | :always) :: %__MODULE__{}
  def reset(error_info), do: reset(error_info, nil)

  def reset(error_info, :exponential) do
    reset_state(error_info, fn value -> value * 2 end)
  end

  def reset(error_info, exponential: [limit: limit]) do
    reset_state(error_info, fn value -> min(value * 2, limit) end)
  end

  def reset(error_info, strategy) when strategy in [:always, nil] do
    reset_state(error_info, nil)
  end

  defp reset_state(error_info, nil) do
    reset_state(error_info, fn _ -> 1 end)
  end

  defp reset_state(error_info, limit_updater_function) do
    %{key: error_hash_key} = error_info

    Agent.get_and_update(
      :boom_notifier,
      fn state ->
        error_storage_item = Map.get(state, error_hash_key)

        state =
          Map.update(
            state,
            error_hash_key,
            default_error_storage(),
            fn error_storage_item ->
              error_storage_item
              |> clear_values()
              |> Map.update!(:__max_storage_capacity__, limit_updater_function)
            end
          )

        {error_storage_item, state}
      end
    )
  end

  defp clear_values(error_storage_item) do
    error_storage_item
    |> Map.replace!(:accumulated_occurrences, 0)
    |> Map.replace!(:first_occurrence, nil)
    |> Map.replace!(:last_occurrence, nil)
  end

  @spec do_send_notification?(__MODULE__.t() | nil) :: boolean()
  defp do_send_notification?(nil), do: false

  defp do_send_notification?(error_storage_item) do
    accumulated_occurrences = Map.get(error_storage_item, :accumulated_occurrences)
    max_storage_capacity = Map.get(error_storage_item, :__max_storage_capacity__)

    accumulated_occurrences >= max_storage_capacity
  end

  defp default_error_storage(timestamp \\ nil) do
    %__MODULE__{
      accumulated_occurrences: 1,
      first_occurrence: timestamp,
      last_occurrence: timestamp,
      __max_storage_capacity__: 1
    }
  end
end
