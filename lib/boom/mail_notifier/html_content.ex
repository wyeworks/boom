defmodule Boom.MailNotifier.HTMLContent do
  import Boom.MailNotifier.Helpers

  def build(%ErrorInfo{controller: nil, stack: stack}) do
    stack_to_html(stack)
  end

  def build(%ErrorInfo{
        name: name,
        controller: controller,
        action: action,
        request: request,
        stack: stack
      }) do
    "<p>#{exception_basic_text(name, controller, action)}</p>" <>
      request_info_to_html(request) <>
      stack_to_html(stack)
  end

  defp request_info_to_html(request) do
    [
      "Request Information:",
      "URL: #{request.url}",
      "Path: #{request.path}",
      "Method: #{request.method}",
      "Port: #{request.port}",
      "Scheme: #{request.scheme}",
      "Query String: #{request.query_string}",
      "Client IP: #{request.client_ip}"
    ]
    |> Enum.map(&wrap_html_item/1)
    |> wrap_html_list
  end

  defp stack_to_html(stack) do
    stack
    |> Enum.map(&stack_entry_to_html/1)
    |> wrap_html_list
  end

  defp stack_entry_to_html(entry) do
    {module, function, arity, [file: file, line: line]} = entry
    left = "<span>#{file}:#{line}</span>"
    right = "<span style=\"float: right\">#{module}.#{function}/#{arity}</span>"

    wrap_html_item(left <> right)
  end

  defp wrap_html_item(html) do
    "<li>#{html}</li>"
  end

  defp wrap_html_list(html) do
    "<ul style=\"list-style-type: none;\">#{html}</ul>"
  end
end
