# Changelog

## [0.7.0] - 2022-02-01

### Additions

*  Add Swoosh support for email notifications ([#74])
*  Provide callback when ErrorHandler is already in use ([#69])

### Fixes/Enhancements

*  Facelift html emails ([#75])

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

[#74]: https://github.com/wyeworks/boom/pull/74
[#75]: https://github.com/wyeworks/boom/pull/75
[#69]: https://github.com/wyeworks/boom/pull/69
[#65]: https://github.com/wyeworks/boom/pull/65
[#64]: https://github.com/wyeworks/boom/pull/64
[#63]: https://github.com/wyeworks/boom/pull/63

[0.7.0]: https://github.com/wyeworks/boom/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/wyeworks/boom/compare/v0.5.0...v0.6.0
