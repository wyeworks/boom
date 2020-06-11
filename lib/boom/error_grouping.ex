defmodule Boom.ErrorGrouping do
  @moduledoc false
  # Keeps track of the errors grouped by type and a counter so the notifier
  # knows the next time it should be executed

  use Agent

  def start_link do
    Agent.start_link(fn -> [] end, name: :boom)
  end

  def update_errors(name, error_info) do
    Agent.update(
      :boom,
      &Keyword.update(&1, name, {1, [error_info]}, fn {count, errors} ->
        {count, errors ++ [error_info]}
      end)
    )

    Agent.get(:boom, fn state -> state end) |> Keyword.get(name)
  end

  def clear_errors(name) do
    Agent.update(
      :boom,
      &Keyword.update!(&1, name, fn {count, _errors} -> {count * 2, []} end)
    )
  end
end
