defmodule Boom.WebhookNotifier do
  @moduledoc """
  Send exception notification as a json using `HTTPoison`.

  ## Usage
  ```elixir
  defmodule YourApp.Router do
  use Phoenix.Router

  use Boom,
    notifier: Boom.WebhookNotifier,
    options: [
      url: "http://example.com"
    ]

  # ...
  ```
  """

  @behaviour Boom.Notifier

  import Boom.Helpers

  @type options :: [{:url, String.t()}]

  @impl Boom.Notifier
  @spec notify(list(%ErrorInfo{}), options) :: no_return()
  def notify(errors_info, url: url) do
    payload =
      errors_info
      |> format_errors()
      |> Jason.encode!()

    HTTPoison.post!(url, payload, [{"Content-Type", "application/json"}])
  end

  defp format_errors(errors) when is_list(errors) do
    Enum.map(errors, &format_error/1)
  end

  defp format_error(%ErrorInfo{
         name: name,
         controller: controller,
         action: action,
         request: request,
         stack: stack
       }) do
    exception_summary =
      if controller && action do
        exception_basic_text(name, controller, action)
      end

    %{
      exception_summary: exception_summary,
      request: request,
      exception_stack_entries: Enum.map(stack, &Exception.format_stacktrace_entry/1)
    }
  end
end
