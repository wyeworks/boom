defmodule ExampleAppWeb.SendNotificationTest do
  use ExUnit.Case, async: true
  use Wallaby.Feature

  def select_email(session, item), do: visit(session, Wallaby.Element.attr(item, "href"))

  def clear_message_queue(session),
    do: click(session, Query.button("Delete Message Queue"))

  feature "Sends notification on error", %{session: session} do
    session
    |> visit("raise-exception")
    |> assert_text("RuntimeError at GET /raise-exception")

    session
    |> visit("/mailbox")
    |> find(Query.css(".list-group"))
    |> find(Query.css(".list-group-item", count: 1), fn item ->
      select_email(session, item)

      # checks email info
      session
      |> find(Query.css(".header-content"))
      |> assert_text("BOOM error caught: Boom")
      |> assert_text("me@example.com")

      # checks email content
      email_body =
        session
        |> find(Query.css(".body-text"))

      assert_text(
        email_body,
        "RuntimeError occurred while the request was processed by PageController#index"
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

    clear_message_queue(session)
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

    clear_message_queue(session)
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
