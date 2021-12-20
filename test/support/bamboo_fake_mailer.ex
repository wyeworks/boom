defmodule Support.BambooFakeMailer do
  @moduledoc false

  # Overrides the Bamboo `deliver_later!/1` function.
  # Instead of sending an email it puts the fields in a mailbox so they can be
  # received in the test

  def deliver_later!(email) do
    send(email.to, {:email_subject, email.subject})
    send(email.to, {:email_from, email.from})
    send(email.to, {:email_to, email.to})
    send(email.to, {:email_text_body, text_body_lines(email.text_body)})
    send(email.to, {:email_html_body, email.html_body})
  end

  defp text_body_lines(body) do
    String.split(body, "\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(String.length(&1) == 0))
  end
end
