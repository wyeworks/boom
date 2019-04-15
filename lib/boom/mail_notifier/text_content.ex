defmodule Boom.MailNotifier.TextContent do
  import Boom.MailNotifier.Helpers, only: [bare_controller_name: 1]

  def build(%ErrorInfo{controller: nil, stack: stack}) do
    stack_to_string(stack)
  end

  def build(%ErrorInfo{controller: controller, stack: stack}) do
    ["Controller: #{bare_controller_name(controller)}\n" | stack_to_string(stack)]
  end

  defp stack_to_string(stack) do
    Enum.map(stack, &(Exception.format_stacktrace_entry(&1) <> "\n"))
  end
end
