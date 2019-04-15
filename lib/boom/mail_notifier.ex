defmodule Boom.MailNotifier do
  @behaviour Boom.Notifier
  import Bamboo.Email

  @impl Boom.Notifier

  @type option ::
          {:mailer, module()} | {:from, String.t()} | {:to, String.t()} | {:subject, String.t()}
  @type options :: [option]

  @spec notify(%ErrorInfo{}, options) :: no_return()
  def notify(error_info, options) do
    [mailer: mailer, from: email_from, to: email_to, subject: subject] = options

    email =
      new_email()
      |> to(email_to)
      |> from(email_from)
      |> subject("#{subject}: #{error_info.reason}")
      |> html_body(html_content(error_info))
      |> text_body(text_content(error_info))

    mailer.deliver_now(email)
  end

  defp text_content(%ErrorInfo{controller: nil, stack: stack}) do
    stack_to_string(stack)
  end

  defp text_content(%ErrorInfo{controller: controller, stack: stack}) do
    ["Controller: #{bare_controller_name(controller)}\n" | stack_to_string(stack)]
  end

  defp html_content(%ErrorInfo{controller: nil, stack: stack}) do
    stack_to_html(stack)
  end

  defp html_content(%ErrorInfo{controller: controller, stack: stack}) do
    "<p>Controller: #{bare_controller_name(controller)}</p>" <> stack_to_html(stack)
  end

  defp bare_controller_name(controller) do
    Atom.to_string(controller)
    |> String.split(".")
    |> Enum.reverse()
    |> List.first()
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
