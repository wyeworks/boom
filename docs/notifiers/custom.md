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
