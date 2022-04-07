if Code.ensure_loaded?(Bamboo) do
  defmodule BoomNotifier.MailNotifier.Bamboo do
    @moduledoc """
    Send exception notification by email using `Bamboo`.

    ## Usage
    ```elixir
    defmodule YourApp.Router do
    use Phoenix.Router

    use BoomNotifier,
        notifier: BoomNotifier.MailNotifier.Bamboo,
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

    alias BoomNotifier.ErrorInfo
    alias BoomNotifier.MailNotifier
    alias BoomNotifier.MailNotifier.HTMLContent
    alias BoomNotifier.MailNotifier.TextContent

    @type option ::
            {:mailer, module()} | {:from, String.t()} | {:to, String.t()} | {:subject, String.t()}
    @type options :: [option]

    @impl BoomNotifier.Notifier
    defdelegate validate_config(options), to: MailNotifier

    @impl BoomNotifier.Notifier
    @spec notify(ErrorInfo.t(), options) :: no_return()
    def notify(error_info, options) do
      subject =
        MailNotifier.build_subject(
          options[:subject],
          error_info,
          options[:max_subject_length] || 80
        )

      email =
        new_email()
        |> to(options[:to])
        |> from(options[:from])
        |> subject(subject)
        |> html_body(HTMLContent.build(error_info))
        |> text_body(TextContent.build(error_info))

      options[:mailer].deliver_later!(email)
    end
  end
end
