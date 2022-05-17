## Email notifier

BoomNotifier has built in support for both [Bamboo](https://github.com/thoughtbot/bamboo) and [Swoosh](https://github.com/swoosh/swoosh).

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
      notifier: BoomNotifier.MailNotifier.Bamboo,
      # or to use Swoosh
      # notifier: BoomNotifier.MailNotifier.Swoosh,
      options: [
        mailer: YourApp.Mailer,
        from: "me@example.com",
        to: "foo@example.com",
        subject: "BOOM error caught"
      ]

  # ...
```

For the email to be sent, you need to define a valid mailer in the `options` keyword list. You can customize the `from`, `to` and `subject` attributes.

`subject` will be truncated at 80 chars, if you want more add the option `max_subject_length`.