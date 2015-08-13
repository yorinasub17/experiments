defmodule Rpc.Supervisor do
  use Supervisor

  @server __MODULE__

  def start_link(port) do
    {:ok, _pid} = Supervisor.start_link(__MODULE__, port, [name: @server])
  end

  def init(port) do
    children = [worker(Rpc.Server, [port], [shutdown: 2000])]
    supervise(children, [strategy: :one_for_one])
  end
end
