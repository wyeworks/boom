defmodule Boom.MailNotifier.HTMLContent do
  import Boom.MailNotifier.Helpers

  def build(%ErrorInfo{controller: nil, stack: stack}) do
    stack_to_html(stack)
  end

  def build(%ErrorInfo{name: name, controller: controller, action: action, stack: stack}) do
    "<p>#{exception_basic_text(name, controller, action)}</p>" <> stack_to_html(stack)
  end

  defp stack_to_html(stack) do
    "<ul style=\"list-style-type: none;\">#{Enum.map(stack, &stack_entry_to_html/1)}</ul>"
  end

  defp stack_entry_to_html(entry) do
    {module, function, arity, [file: file, line: line]} = entry
    left = "<span>#{file}:#{line}</span>"
    right = "<span style=\"float: right\">#{module}.#{function}/#{arity}</span>"

    "<li>#{left}#{right}</li>"
  end
end
