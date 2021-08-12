defmodule BoomNotifier.Application do
  use Application

  @moduledoc false

  def start(_type, _args) do
    import Supervisor.Spec

    children = [
      supervisor(BoomNotifier.ErrorStorage, []),
      {BoomNotifier.NotifierSenderServer, nil}
    ]

    Supervisor.start_link(children, strategy: :one_for_all)
  end
end
