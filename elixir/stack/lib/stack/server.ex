defmodule Stack.Server do
  use GenServer

  ####
  # External API

  def start_link(initial_list) do
    GenServer.start_link(__MODULE__, initial_list, name: __MODULE__)
  end

  def pop do
    GenServer.call(__MODULE__, :pop)
  end

  def push(item) do
    GenServer.cast(__MODULE__, {:push, item})
  end

  ####
  # GenServer implementations

  def handle_call(:pop, _from, [head | tail]) do
    {:reply, head, tail}
  end

  def handle_cast({:push, item}, current_state) do
    {:noreply, [item | current_state]}
  end
end
