defmodule BoomNotifier.DummyMailer do
  @moduledoc """
  A nop mailer for Swoosh and Bamboo
  """
  def deliver_later!(_), do: nil
  def deliver!(_), do: nil
end
