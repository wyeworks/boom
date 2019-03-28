defmodule NotifierTest do
  use ExUnit.Case
  use Plug.Test

  doctest Boom

  defmodule FakeNotifier do
    @behaviour Boom.Notifier

    @impl Boom.Notifier
    def notify(reason, stack, options) do
      subject_prefix = Keyword.get(options, :subject)

      subject = "#{subject_prefix}: #{reason.message}"
      body = Enum.map(stack, &(Exception.format_stacktrace_entry(&1) <> "\n"))

      send(self(), {:subject, subject})
      send(self(), {:body, body})
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

    assert_received {:body,
                     [
                       "test/notifier_test.exs:" <>
                         <<name::binary-size(2),
                           ": NotifierTest.TestPlugSingleNotifier.\"call \(overridable 1\)\"/2\n">>
                       | _
                     ]}
  end

  test "options were passed to multiple notifiers" do
    conn = conn(:get, "/")
    catch_error(TestPlugMultipleNotifiers.call(conn, []))

    assert_received {:subject, "BOOM error caught: booom!"}

    assert_received {:body,
                     [
                       "test/notifier_test.exs:" <>
                         <<name::binary-size(2),
                           ": NotifierTest.TestPlugMultipleNotifiers.\"call \(overridable 1\)\"/2\n">>
                       | _
                     ]}
  end
end
