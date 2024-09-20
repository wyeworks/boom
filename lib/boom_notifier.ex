defmodule BoomNotifier do
  @moduledoc false

  # Responsible for sending a notification to each notifier every time an
  # exception is raised.
  require Logger

  alias BoomNotifier.ErrorInfo
  alias BoomNotifier.NotificationSender

  @spec to_config(Keyword.t() | Atom) :: Keyword.t()
  def to_config(config) when is_atom(config),
    do: config.boom_config()

  def to_config(settings),
    do: settings

  @doc """
  Runs BoomNotifier triggering logic according to the provided configuration which
  can be specified either as a keyword list or as a module atom which uses BoomNotifier.
  """
  @spec notify_error(Keyword.t(), Plug.Conn.t(), error :: any()) :: nil
  def notify_error(settings, conn, %{kind: :error, reason: %mod{}} = error) do
    ignored_exceptions = Keyword.get(to_config(settings), :ignore_exceptions, [])

    unless Enum.member?(ignored_exceptions, mod) do
      trigger_notify_error(settings, conn, error)
    end
  end

  def notify_error(settings, conn, error),
    do: trigger_notify_error(settings, conn, error)

  def walkthrough_notifiers(settings, callback) do
    case Keyword.get(settings, :notifiers) do
      nil ->
        run_callback(settings, callback)

      notifiers_settings when is_list(notifiers_settings) ->
        Enum.each(notifiers_settings, &run_callback(&1, callback))
    end
  end

  def validate_notifiers(notifier, options) do
    if Code.ensure_loaded?(notifier) && function_exported?(notifier, :validate_config, 1) do
      case notifier.validate_config(options) do
        {:error, message} ->
          Logger.error(
            "Notifier validation: #{message} in #{notifier |> to_string() |> String.split(".") |> List.last()}"
          )

        _ ->
          nil
      end
    end
  end

  defp run_callback(settings, callback) do
    if Keyword.has_key?(settings, :notifier) do
      callback.(settings[:notifier], settings[:options])
    else
      Logger.error("Parameter :notifier is missing in #{inspect(settings)}")
    end
  end

  defp trigger_notify_error(settings, conn, error) do
    custom_data = Keyword.get(to_config(settings), :custom_data, :nothing)
    error_info = ErrorInfo.build(error, conn, custom_data)

    NotificationSender.async_trigger_notify(settings, error_info)
  end

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
      BoomNotifier.walkthrough_notifiers(
        config,
        fn notifier, options -> BoomNotifier.validate_notifiers(notifier, options) end
      )

      def notify_error(conn, error) do
        BoomNotifier.notify_error(__MODULE__, conn, error)
      end

      def boom_config do
        unquote(config)
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
