defmodule Boom.WebhookNotifier do
  @behaviour Boom.Notifier

  use HTTPoison.Base

  import Boom.Helpers

  @impl Boom.Notifier
  def notify(error_info, url: url) do
    post!(url, error_info, [{"Content-Type", "application/json"}])
  end

  @impl true
  def process_request_body(body), do: json(body)

  defp json(%ErrorInfo{
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
