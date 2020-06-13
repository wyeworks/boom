defmodule Boom do
  @moduledoc false
  # Notify the exception to all the defined notifiers

  alias Boom.ErrorGrouping

  defmacro __using__(config) do
    quote location: :keep do
      use Plug.ErrorHandler

      import Boom

      defp handle_errors(conn, %{reason: reason, stack: stack} = error) do
        name = Boom.get_reason_name(error)
        error_info = ErrorInfo.build(reason, stack, conn)

        settings = unquote(config)
        {error_grouping, settings} = Keyword.pop(settings, :error_grouping)

        {send_notification?, occurrences} =
          if error_grouping do
            {counter, errors} = ErrorGrouping.update_errors(name, error_info)
            {length(errors) >= counter, errors}
          else
            {true, [error_info]}
          end

        if send_notification? do
          case settings do
            # FIXME: this doesn't match when extra parameters are set
            [notifier: notifier, options: options] ->
              notifier.notify(occurrences, options)

            notifiers_config when is_list(notifiers_config) ->
              for [notifier: notifier, options: options] <- notifiers_config do
                notifier.notify(occurrences, options)
              end
          end

          if error_grouping, do: ErrorGrouping.clear_errors(name)
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
