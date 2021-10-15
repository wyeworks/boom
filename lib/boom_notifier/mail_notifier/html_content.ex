defmodule BoomNotifier.MailNotifier.HTMLContent do
  @moduledoc false

  import BoomNotifier.Helpers
  require EEx

  EEx.function_from_file(
    :def,
    :email_body,
    Path.join([Path.dirname(__ENV__.file), "templates", "email_body.html.eex"]),
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
      exception_stack_entries: Enum.map(stack, &normalize_entry/1),
      timestamp: format_timestamp(timestamp),
      metadata: metadata
    }
  end

  defp normalize_entry({module, fun, arity, location}) do
    {format_application(module), module, fun, arity, location}
  end

  # TODO: Not use copied Exception.format_application/1
  defp format_application(module) do
    # We cannot use Application due to bootstrap issues
    case :application.get_application(module) do
      {:ok, app} ->
        case :application.get_key(app, :vsn) do
          {:ok, vsn} when is_list(vsn) ->
            "(" <> Atom.to_string(app) <> " " <> List.to_string(vsn) <> ") "

          _ ->
            "(" <> Atom.to_string(app) <> ") "
        end

      :undefined ->
        ""
    end
  end

  defp format_timestamp(timestamp) do
    timestamp |> DateTime.truncate(:second) |> DateTime.to_naive() |> NaiveDateTime.to_string()
  end
end
