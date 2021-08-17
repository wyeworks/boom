defmodule BoomNotifier.Application do
  use Application

  @moduledoc false

  def start(_type, _args) do
    children = [
      {
        BoomNotifier.ErrorStorage,
        {BoomNotifier.ErrorStorage, :start_link, []},
        :permanent,
        :infinity,
        :supervisor,
        [BoomNotifier.ErrorStorage]
      },
      BoomNotifier.NotifierSenderServer
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
