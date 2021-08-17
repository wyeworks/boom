defmodule BoomNotifier.Application do
  use Application

  @moduledoc false

  def start(_type, _args) do
    children = [
      %{
        id: BoomNotifier.ErrorStorage,
        start: {BoomNotifier.ErrorStorage, :start_link, []},
        type: :supervisor,
        modules: [BoomNotifier.ErrorStorage]
      },
      BoomNotifier.NotifierSenderServer
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
