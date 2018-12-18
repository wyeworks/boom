defmodule Boom.MailerHelper do
  import Bamboo.Email

  def notify(_) do
    welcome_email =
      new_email()
      |> to("foo@example.com")
      |> from("me@example.com")
      |> subject("Welcome!!!")
      |> html_body("<strong>Welcome</strong>")
      |> text_body("welcome")

    welcome_email |> mailer().deliver_now
  end

  defp mailer do
    Application.fetch_env!(:boom, :mailer)
  end
end
