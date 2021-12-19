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

  alias BoomNotifier.MailNotifier.HTMLContent
  alias BoomNotifier.MailNotifier.TextContent

  @type option ::
          {:mailer, module()} | {:from, String.t()} | {:to, String.t()} | {:subject, String.t()}
  @type options :: [option]

  @impl BoomNotifier.Notifier
  def validate_config(options) do
    missing_keys = Enum.reject([:mailer, :from, :to, :subject], &Keyword.has_key?(options, &1))

    case missing_keys do
      [] -> :ok
      [missing_key] -> {:error, "#{inspect(missing_key)} parameter is missing"}
      _ -> {:error, "The following parameters are missing: #{inspect(missing_keys)}"}
    end
  end

  @impl BoomNotifier.Notifier
  @spec notify(list(%ErrorInfo{}), options) :: no_return()
  def notify(error_info, options) do
    max_subject_length = options[:max_subject_length] || 80

    # Note, unlike Bamboo, Swoosh will raise while creating the mail if it is
    # invalid (has a bad recipient, etc).
    # To consumers of this function, the behaviour is identical to Bamboo's
    # `deliver_later!` because we dispatch the mail in a task.
    email =
      new()
      |> to(options[:to])
      |> from(options[:from])
      |> subject(
        "#{options[:subject]}: #{hd(error_info) |> Map.get(:reason)}"
        |> String.slice(0..(max_subject_length - 1))
      )
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
