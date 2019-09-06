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

  @impl Boom.Notifier
  def notify(error_info, url: url) do
    HTTPoison.post!(url, error_to_json(error_info), [{"Content-Type", "application/json"}])
  end

  defp error_to_json(%ErrorInfo{
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
    |> Poison.encode!()
  end
end
