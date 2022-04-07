defmodule NotifierTest do
  use ExUnit.Case
  use Plug.Test

  import ExUnit.CaptureLog

  doctest BoomNotifier

  @receive_timeout 500

  defmodule FakeNotifier do
    @behaviour BoomNotifier.Notifier

    @impl BoomNotifier.Notifier
    def notify(error_info_list, options) do
      [first_error | _] = error_info_list

      subject_prefix = Keyword.get(options, :subject)
      to_respond_pid = Keyword.get(options, :sender_pid)

      subject = "#{subject_prefix}: #{first_error.reason}"

      body =
        Enum.map(error_info_list, fn error_info ->
          Enum.map(error_info.stack, &(Exception.format_stacktrace_entry(&1) <> "\n"))
        end)

      send(to_respond_pid, %{exception: %{subject: subject, body: body}})
    end
  end

  defmodule FailingNotifier do
    @behaviour BoomNotifier.Notifier

    @impl BoomNotifier.Notifier
    def notify(_, _) do
      raise ArgumentError, message: "invalid argument foo"
    end
  end

  defmodule MissingParameterNotifier do
    @behaviour BoomNotifier.Notifier

    @impl BoomNotifier.Notifier
    def notify(_error_info_list, _options) do
      nil
    end

    @impl BoomNotifier.Notifier
    def validate_config(options) do
      if Keyword.has_key?(options, :parameter) do
        :ok
      else
        {:error, ":parameter parameter is missing"}
      end
    end
  end

  defmodule TestException do
    defexception plug_status: 403, message: "booom!"
  end

  defmodule PlugErrorWithSingleNotifier do
    use BoomNotifier,
      notifier: FakeNotifier,
      options: [
        subject: "BOOM error caught",
        sender_pid: self()
      ]

    def call(_conn, _opts) do
      raise TestException.exception([])
    end
  end

  defmodule PlugErrorWithMultipleNotifiers do
    use BoomNotifier,
      notifiers: [
        [
          notifier: FakeNotifier,
          options: [
            subject: "BOOM error caught",
            sender_pid: self()
          ]
        ]
      ]

    def call(_conn, _opts) do
      raise TestException.exception([])
    end
  end

  defmodule PlugExitTermination do
    use BoomNotifier,
      notifier: FakeNotifier,
      options: [
        subject: "BOOM error caught",
        sender_pid: self()
      ]

    def call(_conn, _opts) do
      exit(:shutdown)
    end
  end

  defmodule PlugThrown do
    use BoomNotifier,
      notifier: FakeNotifier,
      options: [
        subject: "BOOM error caught",
        sender_pid: self()
      ]

    def call(_conn, _opts) do
      throw("thrown error")
    end
  end

  defmodule PlugErrorWithExponentialTriggerNotifier do
    use BoomNotifier,
      notifier: FakeNotifier,
      notification_trigger: :exponential,
      options: [
        subject: "BOOM error caught",
        sender_pid: self()
      ]

    def call(_conn, _opts) do
      raise TestException.exception([])
    end
  end

  defmodule PlugErrorWithExponentialTriggerWithLimitNotifier do
    use BoomNotifier,
      notifier: FakeNotifier,
      notification_trigger: [exponential: [limit: 3]],
      options: [
        subject: "BOOM error caught",
        sender_pid: self()
      ]

    def call(_conn, _opts) do
      raise TestException.exception([])
    end
  end

  defmodule PlugErrorWithFailingNotifier do
    use BoomNotifier,
      notifier: FailingNotifier,
      options: [
        subject: "BOOM error caught"
      ]

    def call(_conn, _opts) do
      raise TestException.exception([])
    end
  end

  setup do
    Agent.update(:boom_notifier, fn _state -> %{} end)
  end

  test "keeps raising an error on exception" do
    conn = conn(:get, "/")

    assert_raise TestException, "booom!", fn ->
      PlugErrorWithSingleNotifier.call(conn, [])
    end
  end

  test "keeps raising an error on process exit" do
    conn = conn(:get, "/")

    assert catch_exit(PlugExitTermination.call(conn, []))
  end

  test "reports exception when options were passed to one notifier" do
    conn = conn(:get, "/")
    catch_error(PlugErrorWithSingleNotifier.call(conn, []))

    assert_receive(
      %{
        exception: %{
          subject: "BOOM error caught: booom!",
          body: [
            [
              "test/notifier_test.exs:" <>
                <<_name::binary-size(2),
                  ": NotifierTest.PlugErrorWithSingleNotifier.\"call \(overridable 1\)\"/2\n">>
              | _
            ]
          ]
        }
      },
      @receive_timeout
    )
  end

  test "reports exception when options were passed to multiple notifiers" do
    conn = conn(:get, "/")
    catch_error(PlugErrorWithMultipleNotifiers.call(conn, []))

    assert_receive(
      %{
        exception: %{
          subject: "BOOM error caught: booom!",
          body: [
            [
              "test/notifier_test.exs:" <>
                <<_name::binary-size(2),
                  ": NotifierTest.PlugErrorWithMultipleNotifiers.\"call \(overridable 1\)\"/2\n">>
              | _
            ]
          ]
        }
      },
      @receive_timeout
    )
  end

  test "reports exit termination" do
    conn = conn(:get, "/")
    catch_exit(PlugExitTermination.call(conn, []))

    assert_receive(%{exception: %{subject: "BOOM error caught: :shutdown"}}, @receive_timeout)
  end

  test "reports thrown error" do
    conn = conn(:get, "/")
    catch_throw(PlugThrown.call(conn, []))

    assert_receive(%{exception: %{subject: "BOOM error caught: thrown error"}}, @receive_timeout)
  end

  test "reports exception in groups when :notification_trigger setting is :exponential" do
    conn = conn(:get, "/")

    catch_error(PlugErrorWithExponentialTriggerNotifier.call(conn, []))
    assert_receive(%{exception: _}, @receive_timeout)

    catch_error(PlugErrorWithExponentialTriggerNotifier.call(conn, []))
    catch_error(PlugErrorWithExponentialTriggerNotifier.call(conn, []))
    assert_receive(%{exception: _}, @receive_timeout)

    {:message_queue_len, exceptions} = Process.info(self(), :message_queue_len)
    assert exceptions == 0
  end

  test "reports exception in groups when :notification_trigger setting is :exponential with limit" do
    conn = conn(:get, "/")

    catch_error(PlugErrorWithExponentialTriggerWithLimitNotifier.call(conn, []))
    assert_receive(%{exception: _}, @receive_timeout)

    catch_error(PlugErrorWithExponentialTriggerWithLimitNotifier.call(conn, []))
    catch_error(PlugErrorWithExponentialTriggerWithLimitNotifier.call(conn, []))
    assert_receive(%{exception: _}, @receive_timeout)

    catch_error(PlugErrorWithExponentialTriggerWithLimitNotifier.call(conn, []))
    catch_error(PlugErrorWithExponentialTriggerWithLimitNotifier.call(conn, []))
    catch_error(PlugErrorWithExponentialTriggerWithLimitNotifier.call(conn, []))
    assert_receive(%{exception: _}, @receive_timeout)

    catch_error(PlugErrorWithExponentialTriggerWithLimitNotifier.call(conn, []))
    catch_error(PlugErrorWithExponentialTriggerWithLimitNotifier.call(conn, []))
    catch_error(PlugErrorWithExponentialTriggerWithLimitNotifier.call(conn, []))
    assert_receive(%{exception: _}, @receive_timeout)

    {:message_queue_len, exceptions} = Process.info(self(), :message_queue_len)
    assert exceptions == 0
  end

  test "reports every exception when :notification_trigger setting is not set" do
    conn = conn(:get, "/")

    catch_error(PlugErrorWithSingleNotifier.call(conn, []))
    assert_receive(%{exception: _}, @receive_timeout)

    catch_error(PlugErrorWithSingleNotifier.call(conn, []))
    assert_receive(%{exception: _}, @receive_timeout)

    catch_error(PlugErrorWithSingleNotifier.call(conn, []))
    assert_receive(%{exception: _}, @receive_timeout)

    {:message_queue_len, exceptions} = Process.info(self(), :message_queue_len)
    assert exceptions == 0
  end

  test "fails silently when there's an exception" do
    conn = conn(:get, "/")

    assert capture_log(fn ->
             assert_raise TestException, "booom!", fn ->
               PlugErrorWithFailingNotifier.call(conn, [])
             end

             Process.sleep(100)
           end) =~
             "An error occurred when sending a notification: ** (ArgumentError) invalid argument foo in NotifierTest.FailingNotifier.notify/2"
  end

  test "logs when parameter in options is missing" do
    :elixir_config.put(:ignore_module_conflict, true)

    conn = conn(:get, "/")

    assert capture_log(fn ->
             defmodule PlugLogWithMissingParameterNotifier do
               use BoomNotifier,
                 notifier: MissingParameterNotifier,
                 options: [
                   another_parameter: "value"
                 ]

               def call(_conn, _opts) do
                 raise TestException.exception([])
               end
             end
           end) =~
             "Notifier validation: :parameter parameter is missing in MissingParameterNotifier"

    :elixir_config.put(:ignore_module_conflict, false)
  end

  test "logs when parameters in config are missing" do
    :elixir_config.put(:ignore_module_conflict, true)

    conn = conn(:get, "/")

    assert capture_log(fn ->
             defmodule PlugLogWithMissingParameterNotifier do
               use BoomNotifier, other: nil

               def call(_conn, _opts) do
                 raise TestException.exception([])
               end
             end
           end) =~ "(BoomNotifier) The following parameters are missing: [:notifier, :options]"

    :elixir_config.put(:ignore_module_conflict, false)
  end

  test "logs when one parameter in config is missing" do
    conn = conn(:get, "/")

    assert capture_log(fn ->
             defmodule PlugLogNotifierWithMissingParameterNotifier do
               use BoomNotifier,
                 options: [
                   parameter: "value"
                 ]

               def call(_conn, _opts) do
                 raise TestException.exception([])
               end
             end
           end) =~ "(BoomNotifier) :notifier parameter is missing"
  end

  describe "ignored exceptions" do
    defmodule PlugErrorIgnoredException do
      use BoomNotifier,
        notifier: FakeNotifier,
        options: [
          subject: "BOOM error caught",
          sender_pid: self()
        ],
        ignore_exceptions: [TestException]

      def call(_conn, _opts) do
        raise TestException.exception([])
      end
    end

    defmodule PlugErrorTrackedException do
      use BoomNotifier,
        notifier: FakeNotifier,
        options: [
          subject: "BOOM error caught",
          sender_pid: self()
        ],
        ignore_exceptions: [TestException]

      def call(_conn, _opts) do
        raise ArgumentError, message: "Wrong argument"
      end
    end

    test "doesn't report exception when module is ignored" do
      conn = conn(:get, "/")

      catch_error(PlugErrorIgnoredException.call(conn, []))
      refute_receive(%{exception: _exception}, @receive_timeout)
    end

    test "reports exception if module is not ignored" do
      conn = conn(:get, "/")

      catch_error(PlugErrorTrackedException.call(conn, []))
      assert_receive(%{exception: _exception}, @receive_timeout)
    end
  end

  describe "custom callback" do
    defmodule PlugErrorWithCallback do
      use Plug.ErrorHandler

      def handle_errors(conn, error) do
        send(self(), :before_callback)
        notify_error(conn, error)
        send(self(), :after_callback)
      end

      use BoomNotifier,
        notifier: FakeNotifier,
        options: [
          sender_pid: self()
        ]

      def call(_conn, _opts) do
        raise TestException.exception([])
      end
    end

    test "sends the notification when handle_errors/2 is defined" do
      conn = conn(:get, "/")

      catch_error(PlugErrorWithCallback.call(conn, []))

      assert_received :before_callback
      assert_receive(%{exception: _exception}, @receive_timeout)
      assert_received :after_callback
    end
  end
end
