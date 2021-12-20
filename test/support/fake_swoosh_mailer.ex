defmodule Support.SwooshFakeMailer do
  @moduledoc false

  defimpl Swoosh.Email.Recipient, for: PID do
    # Swoosh wont accept a raw PID in the `to` field, smudge it into the
    # correct format for unpacking in `deliver!/1`.
    def format(pid) do
      {inspect(pid), pid}
    end
  end

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
    # Swoosh uses a {name, address} tuple for to and from. from `name` will
    # default to `""` when not specified in the email creation.
    # In the tests, we do not care about the name.
    #
    # note tests only currently support one recipient.
    %{to: [{_pid_name, pid}], from: {_, from_addr}} = email

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
