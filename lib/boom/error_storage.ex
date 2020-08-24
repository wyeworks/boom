defmodule Boom.ErrorStorage do
  @moduledoc false
  # Keeps track of the errors grouped by type and a counter so the notifier
  # knows the next time it should be executed

  use Agent

  def start_link do
    Agent.start_link(fn -> %{} end, name: :boom)
  end

  @spec add_errors(atom(), %ErrorInfo{}) :: :ok
  def add_errors(error_reason, error_info) do
    Agent.update(
      :boom,
      &Map.update(&1, error_reason, {1, [error_info]}, fn {counter, errors} ->
        {counter, [error_info | errors]}
      end)
    )
  end

  @spec get_errors(atom()) :: list(%ErrorInfo{})
  def get_errors(error_reason) do
    Agent.get(:boom, fn state -> state end)
    |> Map.get(error_reason)
    |> case do
      nil -> nil
      {_counter, errors} -> errors
    end
  end

  @spec send_notification?(atom()) :: boolean()
  def send_notification?(error_reason) do
    Agent.get(:boom, fn state -> state end)
    |> Map.get(error_reason)
    |> case do
      nil -> false
      {counter, errors} -> length(errors) >= counter
    end
  end

  @type error_strategy :: :always | :exponential | [exponential: [limit: non_neg_integer()]]

  @spec clear_errors(error_strategy, atom()) :: :ok
  def clear_errors(:exponential, error_reason) do
    Agent.update(
      :boom,
      &Map.update!(&1, error_reason, fn {counter, _errors} -> {counter * 2, []} end)
    )
  end

  def clear_errors([exponential: [limit: limit]], error_reason) do
    Agent.update(
      :boom,
      &Map.update!(&1, error_reason, fn {counter, _errors} -> {min(counter * 2, limit), []} end)
    )
  end

  def clear_errors(:always, error_reason) do
    Agent.update(
      :boom,
      &Map.update!(&1, error_reason, fn _value -> {1, []} end)
    )
  end
end
