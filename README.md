# BoomNotifier

[![hex](https://img.shields.io/hexpm/v/boom_notifier?color=78529a)](https://hex.pm/packages/boom_notifier)
[![Elixir CI](https://github.com/wyeworks/boom/actions/workflows/elixir.yml/badge.svg)](https://github.com/wyeworks/boom/actions/workflows/elixir.yml)

---

This package allows your Phoenix application to send notifications whenever
an exceptions is raised. By default it includes an email and a webhook
notifier, but you can implement your custom ones.

It was inspired by the [ExceptionNotification](https://github.com/smartinez87/exception_notification)
gem that provides a similar functionality for Rack/Rails applications.

You can read the full documentation at [https://hexdocs.pm/boom_notifier](https://hexdocs.pm/boom_notifier).

## Installation

The package can be installed by adding `boom_notifier` to your list of dependencies in
`mix.exs`:

```elixir
def deps do
  [
    {:boom_notifier, "~> 0.2.0"}
  ]
end
```

## How to use it

### Webkook notifier

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    notifier: BoomNotifier.WebhookNotifier,
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

For the email to be sent, you need to define a valid mailer in the `options` keyword list. You can customize the `from`, `to` and `subject` attributes.

## Custom notifiers

To create a custom notifier, you need to implement the `BoomNotifier.Notifier` behaviour:

```elixir
@callback notify(list(%ErrorInfo{}), keyword(String.t())) :: no_return()
```

```elixir
defmodule CustomNotifier do
  @behaviour BoomNotifier.Notifier

  @impl BoomNotifier.Notifier
  def notify(errors, options) do
    # ...
    # ...
    # ...
  end
```

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    notifier: CustomNotifier,
    options: [
      # ...
    ]
```

You can also implement an optional callback `validate_config` that receives
the `options` keyword list set in the notifier so the user can be warned
during compilation if the attributes are not correct.

```elixir
@callback validate_config(keyword(String.t())) :: :ok | {:error, String.t()}
```

## Multiple notifiers

BoomNotifier also supports a list of multiple notifiers like in the example below:

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    notifiers: [
      [
        notifier: BoomNotifier.WebhookNotifier,
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

## Notification Trigger
By default, `BoomNotifier` will send a notification every time an exception is
raised.

However, there are different strategies to decide when to send the
notifications using the `:notification_trigger` option with one of the
following values: `:always` and `:exponential`.

### Always
This option is the default one. It will trigger a notification for every
exception.

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    notification_trigger: :always,
    notifiers: [
      # ...
    ]
```

### Exponential
It uses a formula of `log2(errors_count)` to determine whether to send a
notification, based on the accumulated error count for each specific
exception. This makes the notifier only send a notification when the count
is: 1, 2, 4, 8, 16, 32, 64, 128, ..., (2**n).

You can also set an optional max value.

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    notification_trigger: :exponential,
    notifiers: [
      # ...
    ]
```

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    notification_trigger: [exponential: limit: 100]
    notifiers: [
      # ...
    ]
```

## Custom data or Metadata
By default, `BoomNotifier` will **not** include any custom data from your 
requests.

However, there are different strategies to decide which information do
you want to include in the notifications using the `:custom_data` option 
with one of the following values: `:assigns`, `:logger` or both.

The included information will show up in your notification, in a new section 
titled "Metadata".

### Assigns
This option will include the data that is in the [connection](https://hexdocs.pm/plug/Plug.Conn.html)
`assigns` field.

You can also specify the fields you want to retrieve from `conn.assigns`.

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    custom_data: :assigns,
    notifiers: [
      # ...
    ]
```

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    custom_data: [assigns: [fields: [:current_user, :session_data]]],
    notifiers: [
      # ...
    ]
```

Example of adding custom data to the connection:

```elixir
assign(conn, :name, "John")
```

### Logger
This option will include the data that is in the [Logger](https://hexdocs.pm/logger/Logger.html)
`metadata` field.

You can also specify the fields you want to retrieve from `Logger.metadata()`.

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    custom_data: :logger,
    notifiers: [
      # ...
    ]
```

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    custom_data: [logger: [fields: [:request_id, :current_user]]],
    notifiers: [
      # ...
    ]
```

Example of adding custom data to the logger:

```elixir
Logger.metadata(name: "John")
```

### Using both

You can do any combination of the above settings to include data
from both sources.

## License
BoomNotifier is released under the terms of the [MIT License](https://github.com/wyeworks/boom/blob/master/LICENSE).

## Credits
The authors of this project are [Ignacio](https://github.com/iaguirre88) and [Jorge](https://github.com/jmbejar). It is sponsored and maintained by [<img src="https://www.wyeworks.com/images/logo-wbg.svg"/>](https://www.wyeworks.com)
