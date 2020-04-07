defmodule Boom do
  @moduledoc false
  # Notify the exception to all the defined notifiers

  def start_link do
    Agent.start_link(fn -> [] end, name: :boom)
  end

  defmacro __using__(config) do
    quote location: :keep do
      use Plug.ErrorHandler
      use Agent

      import Boom

      defp handle_errors(conn, %{reason: reason, stack: stack} = error) do
        name = Boom.get_reason_name(error)
        error_info = ErrorInfo.build(reason, stack, conn)

        Agent.update(
          :boom,
          &Keyword.update(&1, name, {1, [error_info]}, fn {count, errors} ->
            {count, errors ++ [error_info]}
          end)
        )

        {counter, occurrences} = Agent.get(:boom, fn state -> state end) |> Keyword.get(name)

        settings = unquote(config)
        {error_grouping, settings} = Keyword.pop(settings, :error_grouping)

        send_conditions = !error_grouping || length(occurrences) >= counter

        if send_conditions do
          case settings do
            [notifier: notifier, options: options] ->
              notifier.notify(occurrences, options)

            notifiers_config when is_list(notifiers_config) ->
              for [notifier: notifier, options: options] <- notifiers_config do
                notifier.notify(occurrences, options)
              end
          end

          Agent.update(
            :boom,
            &Keyword.update!(&1, name, fn {count, _errors} -> {count * 2, []} end)
          )
        end
      rescue
        # FIXME: we should handle this in a different way
        # credo:disable-for-next-line
        e -> IO.inspect(e, label: "[Boom] Error sending exception")
      end
    end
  end

  def get_reason_name(%{reason: %name{}}), do: name
  def get_reason_name(%{error: %{kind: kind}}), do: kind
  def get_reason_name(_), do: :error
end
