defmodule Support.SwooshFakeMailer do
  @moduledoc false

  # Overrides the Swoosh `deliver!/1` function.
  #
  # Instead of sending an email it puts the fields in a mailbox so they can be
  # received in the test.
  #
  # Swoosh does include Swoosh.Adapters.Test, but its behaviour seems unreliable
  # in our case because we are catching then sending mail, meaning tests need
  # a lot of Process.sleep/1 calls to wait for mailboxes.
  #
  # It's also possible to use Swoosh.Adapters.Local and
  # Swoosh.Adapter.Local.Storage.Memory which you can directly inspect. This
  # could be used to check for specific mail.

  def deliver!(email) do
    # Swoosh uses a {name, address} tuple for to and from. 
    # The `name` will default to `""` when not specified in the email creation,
    # as is the case in our tests, so extract just the address sections.
    # Note: tests only currently support one recipient.

    %{to: [{_name, "#PID" <> pid_string}], from: {_, from_addr}} = email

    # Since to addresses must be of the correct type, we send the PID through
    # as a string. Convert it back into a true pid() for mailbox delivery.
    pid = :erlang.list_to_pid('#{pid_string}')

    send(pid, {:email_subject, email.subject})
    send(pid, {:email_from, from_addr})
    send(pid, {:email_to, pid})
    send(pid, {:email_text_body, text_body_lines(email.text_body)})
    send(pid, {:email_html_body, email.html_body})
  end

  defp text_body_lines(body) do
    String.split(body, "\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(String.length(&1) == 0))
  end
end
