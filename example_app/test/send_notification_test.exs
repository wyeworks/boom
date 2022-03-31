defmodule ExampleAppWeb.SendNotificationTest do
  use ExUnit.Case, async: true
  use Wallaby.Feature

  feature "Sends notification on error", %{session: session} do
    # raise the exception
    session
    |> visit("/")

    session
    |> visit("/mailbox")
    |> find(Query.css(".list-group"))
    |> find(Query.css(".list-group-item", count: 1), fn item ->
      link = Wallaby.Element.attr(item, "href")

      # click on the email to see its info
      session
      |> visit(link)

      # check email metadata
      session
      |> find(Query.css(".header-content"))
      |> assert_text("BOOM error caught: Boom")
      |> assert_text("me@example.com")

      # check email content
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

    # check notifications are being grouped
    for %{expected_notifications: expected_notifications, expected_emails: expected_emails} <-
          [
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
      |> visit("/")

      items =
        session
        |> visit("/mailbox")
        |> find(Query.css(".list-group"))
        |> find(Query.css(".list-group-item", count: expected_emails))

      item = if expected_emails == 1, do: items, else: List.first(items)
      link = Wallaby.Element.attr(item, "href")

      # click on the email to see its info
      email_page =
        session
        |> visit(link)

      email_body_sections =
        text(email_page, Query.css(".body-text"))
        |> String.split("----------------------------------------")

      assert(length(email_body_sections) - 1 == expected_notifications)
    end
  end
end
