defmodule Boom.MailNotifier do
  @behaviour Boom.Notifier
  import Bamboo.Email

  @impl Boom.Notifier

  @type option ::
          {:mailer, module()} | {:from, String.t()} | {:to, String.t()} | {:subject, String.t()}
  @type options :: [option]

  @spec notify(Boom.Notifier.reason(), [String.t()], options) :: no_return()
  def notify(reason, stack, options) do
    [mailer: mailer, from: email_from, to: email_to, subject: subject] = options

    email =
      new_email()
      |> to(email_to)
      |> from(email_from)
      |> subject("#{subject}: #{reason.message}")
      |> html_body(stack_to_html(stack))
      |> text_body(stack_to_string(stack))

    mailer.deliver_now(email)
  end

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
