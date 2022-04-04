defmodule ExampleAppWeb.SendNotificationTest do
  use ExUnit.Case, async: true
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
    for %{expected_notifications: expected_notifications, expected_emails: expected_emails} <-
          [
            %{expected_notifications: 1, expected_emails: 1},
            %{expected_notifications: 1, expected_emails: 1},
            %{expected_notifications: 2, expected_emails: 2},
            %{expected_notifications: 2, expected_emails: 2},
            %{expected_notifications: 2, expected_emails: 2},
            %{expected_notifications: 2, expected_emails: 2},
            %{expected_notifications: 4, expected_emails: 3},
            %{expected_notifications: 4, expected_emails: 3},
            %{expected_notifications: 4, expected_emails: 3},
            %{expected_notifications: 4, expected_emails: 3},
            %{expected_notifications: 4, expected_emails: 3},
            %{expected_notifications: 4, expected_emails: 3},
            %{expected_notifications: 4, expected_emails: 3},
            %{expected_notifications: 4, expected_emails: 3},
            %{expected_notifications: 8, expected_emails: 4},
            %{expected_notifications: 8, expected_emails: 4},
            %{expected_notifications: 8, expected_emails: 4},
            %{expected_notifications: 8, expected_emails: 4},
            %{expected_notifications: 8, expected_emails: 4},
            %{expected_notifications: 8, expected_emails: 4},
            %{expected_notifications: 8, expected_emails: 4},
            %{expected_notifications: 8, expected_emails: 4},
            %{expected_notifications: 8, expected_emails: 5}
          ] do
      session
      |> visit("/group-exception")
      |> assert_text("GroupExceptionError at GET /group-exception")

      items =
        session
        |> visit("/mailbox")
        |> find(Query.css(".list-group"))
        |> find(Query.css(".list-group-item", count: expected_emails))

      item = if expected_emails == 1, do: items, else: List.first(items)

      email_page = select_email(session, item)

      email_body_sections =
        text(email_page, Query.css(".body-text"))
        |> String.split("----------------------------------------")

      assert(length(email_body_sections) - 1 == expected_notifications)
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
        text: "name: CustomNotifierExceptionError, reason: \"custom notifier exception error\""
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
