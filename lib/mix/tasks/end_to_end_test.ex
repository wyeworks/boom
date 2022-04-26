defmodule Mix.Tasks.EndToEndTest do
  @moduledoc "Runs e2e tests"

  use Mix.Task

  @impl Mix.Task
  def run(_) do
    exit_status = Mix.shell().cmd("cd test/example_app && mix deps.get && mix test")
    exit({:shutdown, exit_status})
  end
end
