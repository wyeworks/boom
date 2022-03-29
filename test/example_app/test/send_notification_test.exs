defmodule ExampleAppWeb.SendNotificationTest do
  use ExUnit.Case, async: true
  use Wallaby.Feature
  alias Wallaby.{Browser, Element, Query}

  feature "sends email when an exception happens", %{session: session} do
    # raise the exception
    session
    |> Browser.visit("/")

    session
    |> Browser.visit("/mailbox")
    |> Browser.find(Query.css(".list-group"))
    |> Browser.find(Query.css(".list-group-item"), fn item ->
      link = Element.attr(item, "href")

      # click on the email to see its info
      session
      |> Browser.visit(link)

      # check email metadata
      session
      |> Browser.find(Query.css(".header-content"))
      |> Browser.assert_text("BOOM error caught: Boom")
      |> Browser.assert_text("me@example.com")

      # check email content
      session
      |> Browser.find(Query.css(".body"))
      |> Browser.assert_text(
        "RuntimeError occurred while the request was processed by PageController#index"
      )
    end)
  end
end
