defmodule SimpleCache.Supervisor do
  use Supervisor

  @server __MODULE__

  def start_link() do
    {:ok, _pid} = Supervisor.start_link(__MODULE__, [], [name: @server])
  end

  def start_child(value, lease_time) do
    Supervisor.start_child(@server, [value, lease_time])
  end

  def init([]) do
    children = [worker(SimpleCache.Element, [], [restart: :temporary, shutdown: :brutal_kill])]
    supervise(children, [strategy: :simple_one_for_one])
  end
end
