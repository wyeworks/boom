defmodule Boom.MailNotifier do
  @behaviour Boom.Notifier
  import Bamboo.Email

  alias Boom.MailNotifier.TextContent
  alias Boom.MailNotifier.HTMLContent

  @impl Boom.Notifier

  @type option ::
          {:mailer, module()} | {:from, String.t()} | {:to, String.t()} | {:subject, String.t()}
  @type options :: [option]

  @spec notify(%ErrorInfo{}, options) :: no_return()
  def notify(error_info, options) do
    [mailer: mailer, from: email_from, to: email_to, subject: subject] = options

    email =
      new_email()
      |> to(email_to)
      |> from(email_from)
      |> subject("#{subject}: #{error_info.reason}")
      |> html_body(HTMLContent.build(error_info))
      |> text_body(TextContent.build(error_info))

    mailer.deliver_now(email)
  end
end
