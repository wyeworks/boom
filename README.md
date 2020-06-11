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

  ...
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

  ...
```

For the email to be sent, you need to define a valid mailer in the `options` keyword list. You can customize the `from`, `to` and `subject` attributes.

## Custom notifiers

To create a custom notifier, you need to implement the `Boom.Notifier` behaviour:

```elixir
@callback notify(%ErrorInfo{}, keyword(String.t())) :: no_return()
```

```elixir
defmodule CustomNotifier do
  @behaviour Boom.Notifier

  @impl Boom.Notifier
  def notify(error_info, options) do
    ...
    ...
    ...
  end
```

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use Boom,
    notifier: CustomNotifier,
    options: [...]
```

## Multiple notifiers

Boom also supports a list of multiple notifiers like in the example below:

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use Boom, [
    [
      notifier: Boom.MailNotifier,
      options: [
        mailer: YourApp.Mailer,
        from: "me@example.com",
        to: "foo@example.com",
        subject: "BOOM error caught"
      ]
    ],
    [
      notifier: CustomNotifier,
      options: [...]
    ],
    ...
  ]
```
