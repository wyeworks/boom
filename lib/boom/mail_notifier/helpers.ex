defmodule Boom.MailNotifier.Helpers do
  def bare_controller_name(controller) do
    Atom.to_string(controller)
    |> String.split(".")
    |> Enum.reverse()
    |> List.first()
  end
end
