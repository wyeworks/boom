defmodule BoomNotifier.MailNotifier do
  @moduledoc """
  Send exception notification by email using `Bamboo`.

  ## Usage
  ```elixir
  defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
      notifier: BoomNotifier.MailNotifier,
      options: [
        mailer: YourApp.Mailer,
        from: "me@example.com",
        to: "foo@example.com",
        subject: "BOOM error caught"
      ]

  # ...
  ```
  """

  @behaviour BoomNotifier.Notifier

  import Bamboo.Email

  alias BoomNotifier.MailNotifier.HTMLContent
  alias BoomNotifier.MailNotifier.TextContent

  @type option ::
          {:mailer, module()} | {:from, String.t()} | {:to, String.t()} | {:subject, String.t()}
  @type options :: [option]

  @impl BoomNotifier.Notifier
  def validate!(options) do
    with _mailer <- Keyword.fetch!(options, :mailer),
         _from <- Keyword.fetch!(options, :from),
         _to <- Keyword.fetch!(options, :to),
         _subject <- Keyword.fetch!(options, :subject) do
      nil
    end
  end

  @impl BoomNotifier.Notifier
  @spec notify(list(%ErrorInfo{}), options) :: no_return()
  def notify(error_info, options) do
    with {:ok, mailer} <- Keyword.fetch(options, :mailer),
         {:ok, email_from} <- Keyword.fetch(options, :from),
         {:ok, email_to} <- Keyword.fetch(options, :to),
         {:ok, subject} <- Keyword.fetch(options, :subject) do
      email =
        new_email()
        |> to(email_to)
        |> from(email_from)
        |> subject("#{subject}: #{hd(error_info) |> Map.get(:reason)}")
        |> html_body(HTMLContent.build(error_info))
        |> text_body(TextContent.build(error_info))

      mailer.deliver_later(email)
    end
  end
end
