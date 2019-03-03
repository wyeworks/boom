defmodule Boom.MailNotifier do
  @behaviour Boom.Notifier
  import Bamboo.Email

  @impl Boom.Notifier
  def create_payload(reason, stack, options) do
    new_email()
    |> to(options)
    |> from(options)
    |> subject("BOOM error caught: #{reason.message}")
    |> html_body(stack_to_html(stack))
    |> text_body(stack_to_string(stack))
  end

  @impl Boom.Notifier
  def notify(email), do: email.deliver_now

  defp stack_to_string(stack) do
    Enum.map(stack, &(Exception.format_stacktrace_entry(&1) <> "\n"))
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
