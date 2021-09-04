defmodule BoomNotifier.WebhookNotifier do
  @moduledoc """
  Send exception notification as a json using `HTTPoison`.

  ## Usage
  ```elixir
  defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    notifier: BoomNotifier.WebhookNotifier,
    options: [
      url: "http://example.com"
    ]

  # ...
  ```
  """

  @behaviour BoomNotifier.Notifier

  import BoomNotifier.Helpers

  @type options :: [{:url, String.t()}]

  @impl BoomNotifier.Notifier
  def validate_config(options) do
    if Keyword.has_key?(options, :url) do
      :ok
    else
      {:error, ":url parameter is missing"}
    end
  end

  @impl BoomNotifier.Notifier
  @spec notify(%ErrorInfo{}, options) :: no_return()
  def notify(error_info, url: url) do
    payload =
      error_info
      |> format_error()
      |> Jason.encode!()

    HTTPoison.post!(url, payload, [{"Content-Type", "application/json"}])
  end

  defp format_error(%ErrorInfo{
         name: name,
         controller: controller,
         action: action,
         request: request,
         stack: stack,
         metadata: metadata,
         first_occurrence: first_occurrence,
         last_occurrence: last_occurrence,
         accumulated_occurrences: accumulated_occurrences
       }) do
    exception_summary =
      if controller && action do
        exception_basic_text(name, controller, action)
      end

    %{
      exception_summary: exception_summary,
      request: request,
      exception_stack_entries: Enum.map(stack, &Exception.format_stacktrace_entry/1),
      metadata: metadata,
      first_occurrence: DateTime.to_iso8601(first_occurrence),
      last_occurrence: DateTime.to_iso8601(last_occurrence),
      accumulated_occurrences: accumulated_occurrences
    }
  end
end
