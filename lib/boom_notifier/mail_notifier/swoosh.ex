if Code.ensure_loaded?(Swoosh) do
  defmodule BoomNotifier.MailNotifier.Swoosh do
    @moduledoc """
    Send exception notification by email using `Swoosh`.

    ## Usage
    ```elixir
    defmodule YourApp.Router do
    use Phoenix.Router

    use BoomNotifier,
        notifier: BoomNotifier.MailNotifier.Swoosh,
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

    import Swoosh.Email

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
      # Note, unlike Bamboo, Swoosh will raise while creating the mail if it is
      # invalid (has a bad recipient, etc).
      # To consumers of this function, the behaviour is identical to Bamboo's
      # `deliver_later!` because we dispatch the mail in a task.

      subject =
        MailNotifier.build_subject(
          options[:subject],
          error_info,
          options[:max_subject_length] || 80
        )

      email =
        new()
        |> to(options[:to])
        |> from(options[:from])
        |> subject(subject)
        |> html_body(HTMLContent.build(error_info))
        |> text_body(TextContent.build(error_info))

      # Swoosh does not provide async send, but it recommends using Elixir tasks.
      # This is actually identical to Bamboo, Bamboo just wraps the boiler plate.
      Task.start(fn ->
        options[:mailer].deliver!(email)
      end)

      # return email to mirror bamboo deliver_later!
      email
    end
  end
end
