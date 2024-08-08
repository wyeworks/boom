defmodule BoomNotifier do
  @moduledoc false

  # Responsible for sending a notification to each notifier every time an
  # exception is raised.
  require Logger

  defmacro __using__(config) do
    quote location: :keep, bind_quoted: [config: config] do
      import BoomNotifier

      error_handler_in_use = Plug.ErrorHandler in @behaviour

      if error_handler_in_use do
        @before_compile BoomNotifier
      else
        use Plug.ErrorHandler

        @impl Plug.ErrorHandler
        def handle_errors(conn, error) do
          notify_error(conn, error)
        end
      end

      # Notifiers validation
      BoomNotifier.Api.walkthrough_notifiers(
        config,
        fn notifier, options -> BoomNotifier.Api.validate_notifiers(notifier, options) end
      )

      def walkthrough_notifiers(callback) do
        unquote(config) |> BoomNotifier.Api.walkthrough_notifiers(callback)
      end

      def notify_error(conn, error) do
        BoomNotifier.Api.notify_error(unquote(config), conn, error)
      end
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      defoverridable handle_errors: 2

      def handle_errors(conn, error) do
        super(conn, error)
        notify_error(conn, error)
      end
    end
  end
end
