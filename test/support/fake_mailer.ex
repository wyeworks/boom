defmodule Support.FakeMailer do
  def deliver_now(email) do
    send(self(), {:email_subject, email.subject})
    send(self(), {:email_from, email.from})
    send(self(), {:email_to, email.to})
    send(self(), {:email_text_body, email.text_body})
    send(self(), {:email_html_body, email.html_body})
  end
end
