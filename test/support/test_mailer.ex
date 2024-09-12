defmodule BoomNotifier.TestMailer do
  @moduledoc """
  Fake mailer for Swoosh and Bamboo
  """

  @doc """
  Bamboo's email deliver that just sends message back
  """
  def deliver_later!(email) do
    # Use email.to to specify test target pid
    email.to
    |> to_pid()
    |> send_back(email.to, email.from, email)
  end

  @doc """
  Swoosh's email deliver that just sends message back
  """
  def deliver!(email) do
    # Swoosh uses a {name, address} tuple for to and from.
    # The `name` will default to `""` when not specified in the email creation,
    # as is the case in our tests, so extract just the address sections.
    # Note: tests only currently support one recipient.

    # this crashes OTP 21, specifically getting the from_addr via tuple match,
    # so get from_addr via elem/2 separately...
    # %{to: [{_name, email_to}], from: {_, from_addr}} = email
    %{to: [{_name, email_to}], from: from} = email
    email_from = elem(from, 1)

    # Use email.to to specify test target pid
    email_to
    |> to_pid()
    |> send_back(email_to, email_from, email)
  end

  defp send_back(nil, _, _, _), do: nil

  defp send_back(pid, email_to, email_from, email) do
    send(pid, {:email_subject, email.subject})
    send(pid, {:email_from, email_from})
    send(pid, {:email_to, email_to})
    send(pid, {:email_text_body, text_body_lines(email.text_body)})
    send(pid, {:email_html_body, email.html_body})
  end

  defp text_body_lines(body) do
    String.split(body, "\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(String.length(&1) == 0))
  end

  defp to_pid(value) do
    value
    |> String.to_existing_atom()
    |> Process.whereis()
  end
end
