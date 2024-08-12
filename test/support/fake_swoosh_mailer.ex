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

    # this crashes OTP 21, specifically getting the from_addr via tuple match,
    # so get from_addr via elem/2 separately...
    # %{to: [{_name, email_to}], from: {_, from_addr}} = email
    %{to: [{_name, email_to}]} = email

    # Use email.to to specify test target pid
    pid =
      email_to
      |> String.to_atom()
      |> Process.whereis()

    send_back(pid, email)
  end

  def send_back(nil, _), do: nil

  def send_back(pid, email) do
    %{to: [{_name, email_to}], from: from} = email
    from_addr = elem(from, 1)

    send(pid, {:email_subject, email.subject})
    send(pid, {:email_from, from_addr})
    send(pid, {:email_to, email_to})
    send(pid, {:email_text_body, text_body_lines(email.text_body)})
    send(pid, {:email_html_body, email.html_body})
  end

  defp text_body_lines(body) do
    String.split(body, "\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(String.length(&1) == 0))
  end
end
