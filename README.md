# BoomNotifier

[![hex](https://img.shields.io/hexpm/v/boom_notifier?color=78529a)](https://hex.pm/packages/boom_notifier)
[![Elixir CI](https://github.com/wyeworks/boom/actions/workflows/elixir.yml/badge.svg)](https://github.com/wyeworks/boom/actions/workflows/elixir.yml)

---

This package allows your Phoenix application to send notifications whenever
an exception is raised. By default it includes an email and a webhook
notifier, but you can implement custom ones.

It was inspired by the [ExceptionNotification](https://github.com/smartinez87/exception_notification)
gem that provides a similar functionality for Rack/Rails applications.

You can read the full documentation at [https://hexdocs.pm/boom_notifier](https://hexdocs.pm/boom_notifier).

## Installation

The package can be installed by adding `boom_notifier` to your list of dependencies in
`mix.exs`:

```elixir
def deps do
  [
    {:boom_notifier, "~> 0.7.0"}
  ]
end
```

## How to use it

### Webhook notifier

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    notifier: BoomNotifier.WebhookNotifier,
    options: [
      url: "http://example.com",
      headers: [Authorization: "Bearer token"]
    ]

  # ...
```

To configure it, you need to set the `url` in the `options` keyword list. A `POST` request with a `json` will be made to that webhook when an error ocurrs with the relevant information.
Optionally, you could also add `headers` to the request as another keyword list.

### Email notifier

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

## Setup when `handle_errors/2` is already being used

If you are using or want to use your own implementation of `handle_errors/2` for the` Plug.ErrorHandler` module, be sure to include the usage of `BoomNotifier` after
that.

In addition, you will have to add the `notify_error/2` callback that `BoomNotifier` provides within your implementation of `handle_errors/2`.

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use Plug.ErrorHandler

  def handle_errors(conn, error) do
    # ...
    notify_error(conn, error)
    # ...
  end

  use BoomNotifier,
    ...
```

## Custom notifiers

To create a custom notifier, you need to implement the `BoomNotifier.Notifier` behaviour:

```elixir
@callback notify(%ErrorInfo{}, keyword(String.t())) :: no_return()
```

`ErrorInfo` is a struct that contains the following attributes:

* `name`: the error name.
* `reason`: the error reason.
* `stack`: the error stacktrace.
* `controller`: the controller where the exception occurred.
* `action`: the action in the controller that failed.
* `request`: the request information that caused the exception.
* `timestamp`: the UTC time when the exception happened.
* `metatadata`: assigns and logger metadata.
* `occurrences`: aggregated information about the errors that are being
  grouped.
  * `accumulated_occurrences`: how many times an exception occurred before the
  notification was sent.
  * `first_occurrence`: collects the time when the first exception was raised.
  * `last_occurrence`: collects the time when the last exception was raised.

Then you can use that information in your `notify/2` implementation.

```elixir
defmodule CustomNotifier do
  @behaviour BoomNotifier.Notifier

  @impl BoomNotifier.Notifier
  def notify(error, options) do
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
is: 1, 2, 4, 8, 16, 32, 64, 128, ..., (2\*\*n).

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
    notification_trigger: [exponential: [limit: 100]]
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
from both sources. The names of the fields are independent for each
source, they will appear under the source namespace.

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    custom_data: [
      [assigns: [fields: [:current_user]]],
      [logger: [fields: [:request_id, :current_user]]]
    ],
    notifiers: [
      # ...
    ]
   # ...
end
```

## Ignore exceptions

By default, all exceptions are captured by Boom. The `:ignore_exceptions` setting is provided to ignore exceptions of a certain kind. Said exceptions will not generate any kind of notification from Boom.

```elixir
defmodule YourApp.Router do
  use Phoenix.Router

  use BoomNotifier,
    ignore_exceptions: [
      HTTPoison.Error, MyApp.CustomException
    ],
    notifiers: [
      # ...
    ]
   # ...
end
```

## License

BoomNotifier is released under the terms of the [MIT License](https://github.com/wyeworks/boom/blob/master/LICENSE).

## Credits

The authors of this project are [Ignacio](https://github.com/iaguirre88) and [Jorge](https://github.com/jmbejar). It is sponsored and maintained by [Wyeworks](https://www.wyeworks.com).
