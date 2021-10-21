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
    max_subject_length = if options[:max_subject_length] do
      options[:max_subject_length]
    else
      254
    end
    
    email =
      new_email()
      |> to(options[:to])
      |> from(options[:from])
      |> subject("#{options[:subject]}: #{hd(error_info) |> Map.get(:reason)}" |> String.slice(0..max_subject_length))
      |> html_body(HTMLContent.build(error_info))
      |> text_body(TextContent.build(error_info))

    options[:mailer].deliver_later!(email)
  end
end
