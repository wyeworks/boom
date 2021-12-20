defmodule Support.BambooFakeMailer do
  @moduledoc false

  # Overrides the Bamboo `deliver_later!/1` function.
  # Instead of sending an email it puts the fields in a mailbox so they can be
  # received in the test

  def deliver_later!(email) do
    # Since to addresses must be of the correct type, we send the PID through
    # as a string. Convert it back into a true pid() for mailbox delivery.
    "#PID" <> pid_string = email.to
    pid = :erlang.list_to_pid('#{pid_string}')

    send(pid, {:email_subject, email.subject})
    send(pid, {:email_from, email.from})
    send(pid, {:email_to, email.to})
    send(pid, {:email_text_body, text_body_lines(email.text_body)})
    send(pid, {:email_html_body, email.html_body})
  end

  defp text_body_lines(body) do
    String.split(body, "\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(String.length(&1) == 0))
  end
end
