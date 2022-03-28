defmodule ExampleAppWeb.SendNotificationTest do
  use ExUnit.Case, async: true
  use Wallaby.Feature
  import Wallaby.Query

  feature "sends email when an exception happens", %{session: session} do
    session
    |> visit("/")
    |> find(css("section.phx-hero"))
    |> assert_has(css("h1", text: "Welcome to Phoenix!"))
  end
end
