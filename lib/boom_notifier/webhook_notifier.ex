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
      url: "http://example.com",
      headers: [Authorization: "Bearer token"]
    ]

  # ...
  ```
  """

  @behaviour BoomNotifier.Notifier

  import BoomNotifier.Helpers

  @type options ::
          [{:url, String.t()}] | [{:url, String.t()}, {:headers, HTTPoison.Request.headers()}]

  @impl BoomNotifier.Notifier
  def validate_config(options) do
    if Keyword.has_key?(options, :url) do
      :ok
    else
      {:error, ":url parameter is missing"}
    end
  end

  @impl BoomNotifier.Notifier
  @spec notify(ErrorInfo.t(), options) :: no_return()
  def notify(error_info, options) do
    payload =
      error_info
      |> format_error()
      |> Jason.encode!()

    headers =
      options
      |> Keyword.get(:headers, [])
      |> Keyword.merge("Content-Type": "application/json")

    HTTPoison.post!(options[:url], payload, headers)
  end

  defp format_error(%ErrorInfo{
         name: name,
         controller: controller,
         action: action,
         request: request,
         stack: stack,
         timestamp: timestamp,
         metadata: metadata
       }) do
    exception_summary =
      if controller && action do
        exception_basic_text(name, controller, action)
      end

    %{
      exception_summary: exception_summary,
      request: request,
      exception_stack_entries: Enum.map(stack, &Exception.format_stacktrace_entry/1),
      timestamp: DateTime.to_iso8601(timestamp),
      metadata: metadata
    }
  end
end
