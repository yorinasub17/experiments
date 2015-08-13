defmodule Rpc do
  use Application

  def start(_type, port) do
    Rpc.Supervisor.start_link(port)
  end
end
