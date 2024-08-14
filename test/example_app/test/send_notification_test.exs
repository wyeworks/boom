defmodule ExampleAppWeb.SendNotificationTest do
  use BoomNotifier.Case, async: true
  use Wallaby.Feature

  def select_email(session, item), do: visit(session, Wallaby.Element.attr(item, "href"))

  def clear_message_queue(session) do
    mailbox_page = visit(session, "/mailbox")
    clear_emails_button_query = Query.button("Delete Message Queue")

    if has?(mailbox_page, clear_emails_button_query),
      do: click(mailbox_page, clear_emails_button_query)
  end

  setup %{session: session} do
    clear_message_queue(session)
    :ok
  end

  feature "Sends notification on error", %{session: session} do
    session
    |> visit("raise-exception")
    |> assert_text("CustomExceptionError at GET /raise-exception")

    session
    |> visit("/mailbox")
    |> find(Query.css(".list-group"))
    |> find(Query.css(".list-group-item", count: 1), fn item ->
      select_email(session, item)

      # checks email info
      session
      |> find(Query.css(".header-content"))
      |> assert_text("BOOM error caught: custom exception error")
      |> assert_text("me@example.com")

      # checks email content
      email_body =
        session
        |> find(Query.css(".body-text"))

      assert_text(
        email_body,
        "CustomExceptionError occurred while the request was processed by PageController#index"
      )

      # includes metadata
      assert_text(
        email_body,
        "assign_metadata: test_assign_metadata"
      )

      assert_text(
        email_body,
        "logger_metadata: test_assign_metadata"
      )
    end)
  end

  feature "Sends notification in groups", %{session: session} do
    for %{accumulated_occurrences: accumulated_occurrences, expected_emails: expected_emails} <-
          [
            %{accumulated_occurrences: 1, expected_emails: 1},
            %{accumulated_occurrences: 1, expected_emails: 1},
            %{accumulated_occurrences: 2, expected_emails: 2},
            %{accumulated_occurrences: 2, expected_emails: 2},
            %{accumulated_occurrences: 2, expected_emails: 2},
            %{accumulated_occurrences: 2, expected_emails: 2},
            %{accumulated_occurrences: 4, expected_emails: 3},
            %{accumulated_occurrences: 4, expected_emails: 3},
            %{accumulated_occurrences: 4, expected_emails: 3},
            %{accumulated_occurrences: 4, expected_emails: 3},
            %{accumulated_occurrences: 4, expected_emails: 3},
            %{accumulated_occurrences: 4, expected_emails: 3},
            %{accumulated_occurrences: 4, expected_emails: 3},
            %{accumulated_occurrences: 4, expected_emails: 3},
            %{accumulated_occurrences: 8, expected_emails: 4},
            %{accumulated_occurrences: 8, expected_emails: 4},
            %{accumulated_occurrences: 8, expected_emails: 4},
            %{accumulated_occurrences: 8, expected_emails: 4},
            %{accumulated_occurrences: 8, expected_emails: 4},
            %{accumulated_occurrences: 8, expected_emails: 4},
            %{accumulated_occurrences: 8, expected_emails: 4},
            %{accumulated_occurrences: 8, expected_emails: 4},
            %{accumulated_occurrences: 8, expected_emails: 5}
          ] do
      session
      |> visit("/group-exception")
      |> assert_text("GroupExceptionError at GET /group-exception")

      items =
        session
        |> visit("/mailbox")
        |> find(Query.css(".list-group"))
        |> find(Query.css(".list-group-item", count: expected_emails))

      if expected_emails > 1 do
        item = List.first(items)
        email = select_email(session, item)

        email
        |> find(Query.css(".body-text"))
        |> assert_text("Errors: #{accumulated_occurrences}")
      end
    end
  end

  feature "Errors were sent to a custom notifier too", %{session: session} do
    session
    |> visit("custom-notifier-exception")

    # exception is sent to the custom notifier
    session
    |> visit("/check-custom-notifier")
    |> assert_has(
      Query.css(".error",
        text: "name: CustomNotifierExceptionError"
      )
    )

    # exception is sent to the mail notifier
    session
    |> visit("/mailbox")
    |> find(Query.css(".list-group"))
    |> assert_text("BOOM error caught: custom notifier exception error")
  end

  feature "Ignores certain errors", %{session: session} do
    session
    |> visit("ignore-exception")
    |> assert_text("IgnoreExceptionError at GET /ignore-exception")

    session
    |> visit("/mailbox")
    |> find(Query.css(".list-group"))
    |> assert_text("Empty mailbox...")
  end
end
