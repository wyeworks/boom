defmodule Boom.MailNotifier.TextContent do
  @moduledoc false

  import Boom.Helpers

  def build(%ErrorInfo{
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

    EEx.eval_file(template_path(),
      exception_summary: exception_summary,
      request: request,
      exception_stack_entries: Enum.map(stack, &Exception.format_stacktrace_entry/1)
    )
  end

  defp template_path do
    current_folder_path = Path.dirname(__ENV__.file)
    Path.join([current_folder_path, "templates", "email_body.text.eex"])
  end
end
