defmodule BoomNotifier.Application do
  use Application

  @moduledoc false

  def start(_type, _args) do
    children = [
      BoomNotifier.ErrorStorage,
      BoomNotifier.NotifierSenderServer,
      {Task.Supervisor, name: BoomNotifier.AsyncEmailSupervisor}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
