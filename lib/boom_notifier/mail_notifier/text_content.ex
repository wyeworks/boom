defmodule BoomNotifier.MailNotifier.TextContent do
  @moduledoc false

  import BoomNotifier.Helpers

  def build(errors) when is_list(errors) do
    EEx.eval_file(template_path(), errors: Enum.map(errors, &build/1))
  end

  def build(%ErrorInfo{
        name: name,
        controller: controller,
        action: action,
        request: request,
        stack: stack,
        timestamp: timestamp
      }) do
    exception_summary =
      if controller && action do
        exception_basic_text(name, controller, action)
      end

    %{
      exception_summary: exception_summary,
      request: request,
      exception_stack_entries: Enum.map(stack, &Exception.format_stacktrace_entry/1),
      timestamp: format_timestamp(timestamp)
    }
  end

  defp template_path do
    current_folder_path = Path.dirname(__ENV__.file)
    Path.join([current_folder_path, "templates", "email_body.text.eex"])
  end

  defp format_timestamp(timestamp) do
    timestamp |> DateTime.truncate(:second) |> DateTime.to_naive() |> NaiveDateTime.to_string()
  end
end
