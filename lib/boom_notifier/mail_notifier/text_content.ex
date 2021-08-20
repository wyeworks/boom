defmodule BoomNotifier.MailNotifier.TextContent do
  @moduledoc false

  import BoomNotifier.Helpers
  require EEx

  EEx.function_from_file(
    :def,
    :email_body,
    Path.join([Path.dirname(__ENV__.file), "templates", "email_body.text.eex"]),
    [:errors]
  )

  def build(errors) when is_list(errors) do
    email_body(Enum.map(errors, &build/1))
  end

  def build(%ErrorInfo{
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
      timestamp: format_timestamp(timestamp),
      metadata: metadata
    }
  end

  defp format_timestamp(timestamp) do
    timestamp |> DateTime.truncate(:second) |> DateTime.to_naive() |> NaiveDateTime.to_string()
  end
end
