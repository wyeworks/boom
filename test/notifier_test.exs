defmodule NotifierTest do
  use ExUnit.Case
  use Plug.Test

  doctest Boom

  defmodule FakeNotifier do
    @behaviour Boom.Notifier

    @impl Boom.Notifier
    def notify(error_info_list, options) do
      [first_error | _] = error_info_list
      subject_prefix = Keyword.get(options, :subject)
      subject = "#{subject_prefix}: #{first_error.reason}"

      body =
        Enum.map(error_info_list, fn error_info ->
          Enum.map(error_info.stack, &(Exception.format_stacktrace_entry(&1) <> "\n"))
        end)

      send(self(), %{exception: %{subject: subject, body: body}})
    end
  end

  defmodule TestException do
    defexception plug_status: 403, message: "booom!"
  end

  defmodule PlugErrorWithSingleNotifier do
    use Boom,
      notifier: FakeNotifier,
      options: [
        subject: "BOOM error caught"
      ]

    def call(_conn, _opts) do
      raise TestException.exception([])
    end
  end

  defmodule PlugErrorWithMultipleNotifiers do
    use Boom, [
      [
        notifier: FakeNotifier,
        options: [
          subject: "BOOM error caught"
        ]
      ]
    ]

    def call(_conn, _opts) do
      raise TestException.exception([])
    end
  end

  defmodule PlugExitTermination do
    use Boom,
      notifier: FakeNotifier,
      options: [
        subject: "BOOM error caught"
      ]

    def call(_conn, _opts) do
      exit(:shutdown)
    end
  end

  defmodule PlugThrown do
    use Boom,
      notifier: FakeNotifier,
      options: [
        subject: "BOOM error caught"
      ]

    def call(_conn, _opts) do
      throw("thrown error")
    end
  end

  defmodule PlugErrorWithErrorGroupingNotifier do
    use Boom,
      notifier: FakeNotifier,
      error_grouping: true,
      options: [
        subject: "BOOM error caught"
      ]

    def call(_conn, _opts) do
      raise TestException.exception([])
    end
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

    assert_received %{
      exception: %{
        subject: "BOOM error caught: booom!",
        body: [
          [
            "test/notifier_test.exs:" <>
              <<name::binary-size(2),
                ": NotifierTest.PlugErrorWithSingleNotifier.\"call \(overridable 1\)\"/2\n">>
            | _
          ]
        ]
      }
    }
  end

  test "reports exception when options were passed to multiple notifiers" do
    conn = conn(:get, "/")
    catch_error(PlugErrorWithMultipleNotifiers.call(conn, []))

    assert_received %{
      exception: %{
        subject: "BOOM error caught: booom!",
        body: [
          [
            "test/notifier_test.exs:" <>
              <<name::binary-size(2),
                ": NotifierTest.PlugErrorWithMultipleNotifiers.\"call \(overridable 1\)\"/2\n">>
            | _
          ]
        ]
      }
    }
  end

  test "reports exit termination" do
    conn = conn(:get, "/")
    catch_exit(PlugExitTermination.call(conn, []))

    assert_received %{exception: %{subject: "BOOM error caught: :shutdown"}}
  end

  test "reports thrown error" do
    conn = conn(:get, "/")
    catch_throw(PlugThrown.call(conn, []))

    assert_received %{exception: %{subject: "BOOM error caught: thrown error"}}
  end

  test "reports exception in groups when :error_grouping setting is enabled" do
    conn = conn(:get, "/")

    catch_error(PlugErrorWithErrorGroupingNotifier.call(conn, []))
    assert_received %{exception: _}

    catch_error(PlugErrorWithErrorGroupingNotifier.call(conn, []))
    catch_error(PlugErrorWithErrorGroupingNotifier.call(conn, []))
    assert_received %{exception: _}

    {:message_queue_len, exceptions} = Process.info(self(), :message_queue_len)
    assert exceptions == 0
  end

  test "reports every exception when :error_grouping setting is disabled" do
    conn = conn(:get, "/")

    catch_error(PlugErrorWithSingleNotifier.call(conn, []))
    assert_received %{exception: _}

    catch_error(PlugErrorWithSingleNotifier.call(conn, []))
    assert_received %{exception: _}

    catch_error(PlugErrorWithSingleNotifier.call(conn, []))
    assert_received %{exception: _}

    {:message_queue_len, exceptions} = Process.info(self(), :message_queue_len)
    assert exceptions == 0
  end
end
