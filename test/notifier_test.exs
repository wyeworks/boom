defmodule NotifierTest do
  use ExUnit.Case
  use Plug.Test

  doctest Boom

  defmodule FakeNotifier do
    @behaviour Boom.Notifier

    @impl Boom.Notifier
    def create_payload(reason, stack, options) do
      subject_prefix = Keyword.get(options, :subject)

      %{
        subject: "#{subject_prefix}: #{reason.message}",
        body: Enum.map(stack, &(Exception.format_stacktrace_entry(&1) <> "\n"))
      }
    end

    @impl Boom.Notifier
    def notify(payload) do
      send(self(), {:subject, payload.subject})
      send(self(), {:body, payload.body})
    end
  end

  defmodule TestException do
    defexception plug_status: 403, message: "booom!"
  end

  defmodule TestPlugSingleNotifier do
    use Boom,
      notifier: FakeNotifier,
      options: [
        subject: "BOOM error caught"
      ]

    def call(_conn, _opts) do
      raise TestException.exception([])
    end
  end

  defmodule TestPlugMultipleNotifiers do
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

  test "Raising an error on failure" do
    conn = conn(:get, "/")

    assert_raise TestException, "booom!", fn ->
      TestPlugSingleNotifier.call(conn, [])
    end
  end

  test "options were passed to the notifier" do
    conn = conn(:get, "/")
    catch_error(TestPlugSingleNotifier.call(conn, []))

    assert_received {:subject, "BOOM error caught: booom!"}

    receive do
      {:body, [first_line | _]} ->
        expectation =
          ~r{test/notifier_test.exs:\d+: NotifierTest.TestPlugSingleNotifier."call \(overridable 1\)"/2\n}

        assert Regex.match?(expectation, first_line)
    end
  end

  test "options were passed to multiple notifiers" do
    conn = conn(:get, "/")
    catch_error(TestPlugMultipleNotifiers.call(conn, []))

    assert_received {:subject, "BOOM error caught: booom!"}

    receive do
      {:body, [first_line | _]} ->
        expectation =
          ~r{test/notifier_test.exs:\d+: NotifierTest.TestPlugMultipleNotifiers."call \(overridable 1\)"/2\n}

        assert Regex.match?(expectation, first_line)
    end
  end
end
