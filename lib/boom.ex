defmodule Boom do
  @moduledoc false
  # Notify the exception to all the defined notifiers

  alias Boom.ErrorStorage

  defmodule ConfigException do
    defexception message: "Boom configuration error encountered"
  end

  defmacro __using__(config) do

    with :ok <- validate_config(config) do    
      quote location: :keep do
        use Plug.ErrorHandler

        import Boom

        def handle_errors(conn, error) do
          {error_kind, error_info} = ErrorInfo.build(error, conn)

          ErrorStorage.add_errors(error_kind, error_info)

          if ErrorStorage.send_notification?(error_kind) do
            occurrences = ErrorStorage.get_errors(error_kind)

            settings = unquote(config)

            case Keyword.get(settings, :notifiers) do
              nil ->
                with {:ok, notifier} <- Keyword.fetch(settings, :notifier),
                    {:ok, options} <- Keyword.fetch(settings, :options) do
                  notifier.notify(occurrences, options)
                end

              notifiers_config when is_list(notifiers_config) ->
                for notifier_config <- notifiers_config do
                  with {:ok, notifier} <- Keyword.fetch(notifier_config, :notifier),
                      {:ok, options} <- Keyword.fetch(notifier_config, :options) do
                    notifier.notify(occurrences, options)
                  end
                end
            end

            {notification_trigger, _settings} =
              Keyword.pop(settings, :notification_trigger, :always)

            ErrorStorage.clear_errors(notification_trigger, error_kind)
          end
        rescue
          # FIXME: we should handle this in a different way
          # credo:disable-for-next-line
          e -> IO.inspect(e, label: "[Boom] Error sending exception")
        end
      end
  end
end

  defp validate_config(config) do
    with  {:validate_notifier, {:ok, _notifier}} <- {:validate_notifier, Keyword.fetch(config, :notifier)},
          {:validate_options, {:ok, _options}} <- {:validate_options, Keyword.fetch(config, :options)} do
      :ok
    else 
      {:validate_notifier, _} -> raise ConfigException, message: "No notifier found"
      {:validate_options, _} -> raise ConfigException, message: "No options map found"
    end
  end
end
