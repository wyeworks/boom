# Boom

This package allows your Phoenix application to send notifications whenever
an exceptions is raised. By default it includes an email and a webhook
notifier, but you can implement your custom ones.

It was inspired by the [ExceptionNotification](https://github.com/smartinez87/exception_notification)
gem that provides a similar functionality for Rack/Rails applications.

## Installation

The package can be installed by adding `boom` to your list of dependencies in
`mix.exs`:

```elixir
def deps do
  [
    {:boom, "~> 0.1.0"}
  ]
end
```

## How to use it

### Webkook notifier

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use Boom,
    notifier: Boom.WebhookNotifier,
    options: [
      url: "http://example.com"
    ]

  # ...
```

To configure it, you need to set the `url` in the `options` keyword list. A `POST` request with a `json` will be made to that webhook when an error ocurrs with the relevant information.

### Email notifier

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use Boom,
      notifier: Boom.MailNotifier,
      options: [
        mailer: YourApp.Mailer,
        from: "me@example.com",
        to: "foo@example.com",
        subject: "BOOM error caught"
      ]

  # ...
```

For the email to be sent, you need to define a valid mailer in the `options` keyword list. You can customize the `from`, `to` and `subject` attributes.

## Custom notifiers

To create a custom notifier, you need to implement the `Boom.Notifier` behaviour:

```elixir
@callback notify(list(%ErrorInfo{}), keyword(String.t())) :: no_return()
```

```elixir
defmodule CustomNotifier do
  @behaviour Boom.Notifier

  @impl Boom.Notifier
  def notify(errors, options) do
    # ...
    # ...
    # ...
  end
```

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use Boom,
    notifier: CustomNotifier,
    options: [
      # ...
    ]
```

## Multiple notifiers

Boom also supports a list of multiple notifiers like in the example below:

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use Boom,
    notifiers: [
      [
        notifier: Boom.WebhookNotifier,
        options: [
          url: "http://example.com",
        ]
      ],
      [
        notifier: CustomNotifier,
        options: # ...
      ]
    ]
```

## Error grouping
By default, `Boom` will send a notification every time an exception is
raised. To prevent this, you can choose to group errors by setting the
`:error_grouping` option to `true`.

Error grouping uses a formula of `log2(errors_count)` to determine whether to
send the notification, based on the accumulated error count for each specific
exception. This makes the notifier only send a notification when the count
is: 1, 2, 4, 8, 16, 32, 64, 128, ..., (2**n).

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use Boom,
    error_grouping: true,
    notifiers: [
      # ...
    ]
```
