# Changelog

## [0.8.0] - 2022-04-26

### Fixes/Enhancements
* Tell the user where the option key was missing from ([#77])

### Breaking changes
* We stopped sending an error list to the notifiers. Instead of sending every
 error in the notification we send the following aggregated data:
 `accumulated_occurrences`, `first_occurrence` and `last_occurrence`.
 If you are implementing a custom one you have to change the `notify/2`
 function accordingly ([#79])
* Drop support for Elixir 1.7 ([#72])

## [0.7.0] - 2022-02-01

### Additions

* Add Swoosh support for email notifications ([#74])
* Provide callback when ErrorHandler is already in use ([#69])

### Fixes/Enhancements

* Facelift html emails ([#75])

### Breaking changes
* Now you should set the email delivery library you want to use in the
  configuration. We currently support Bamboo or Swoosh.

  In case you want to keep using Bamboo you should change
  `BoomNotifier.MailNotifier` to `BoomNotifier.MailNotifier.Bamboo`.

    ```elixir
      use BoomNotifier,
          notifier: BoomNotifier.MailNotifier.Bamboo,
          options: [
            mailer: YourApp.Mailer,
            ...
          ]
    ```

    In case you want to start using Swoosh you should change
    `BoomNotifier.MailNotifier` to `BoomNotifier.MailNotifier.Swoosh`.
    ```elixir
      use BoomNotifier,
          notifier: BoomNotifier.MailNotifier.Swoosh,
          options: [
            mailer: YourApp.Mailer,
            ...
          ]
    ```

## [0.6.0] - 2021-10-29

### Additions

* Add the ability to ignore exceptions ([#63])

### Fixes/Enhancements

* Mail notifier subject length issue ([#65])
* Attempt to improve legibility of stacktrace with monospace font ([#64])

[#79]: https://github.com/wyeworks/boom/pull/79
[#77]: https://github.com/wyeworks/boom/pull/77
[#72]: https://github.com/wyeworks/boom/pull/72
[#74]: https://github.com/wyeworks/boom/pull/74
[#75]: https://github.com/wyeworks/boom/pull/75
[#69]: https://github.com/wyeworks/boom/pull/69
[#65]: https://github.com/wyeworks/boom/pull/65
[#64]: https://github.com/wyeworks/boom/pull/64
[#63]: https://github.com/wyeworks/boom/pull/63

[0.8.0]: https://github.com/wyeworks/boom/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/wyeworks/boom/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/wyeworks/boom/compare/v0.5.0...v0.6.0
