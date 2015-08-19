defmodule SimpleCache.Element do
  use GenServer

  @server __MODULE__
  @default_lease_time (60*60*24)

  defmodule State do
    defstruct value: nil, lease_time: nil, start_time: 0
  end
  @type state :: %State{value: any, lease_time: integer, start_time: any}

  # API
  def start_link(value, lease_time) do
    GenServer.start_link(__MODULE__, [value, lease_time])
  end

  def create(value, lease_time) do
    SimpleCache.Supervisor.start_child(value, lease_time)
  end

  def create(value) do
    create(value, @default_lease_time)
  end

  def fetch(pid) do
    GenServer.call(pid, :fetch)
  end

  def replace(pid, value) do
    GenServer.cast(pid, {:replace, value})
  end

  def delete(pid) do
    GenServer.cast(pid, :delete)
  end

  # GenServer implementation
  def init([value, lease_time]) do
    now = Timex.Date.now
    {:ok, %State{value: value, lease_time: lease_time, start_time: now}, time_left(now, lease_time)}
  end

  @doc """
  Returns the amount of time left given the start time and the lease time, in
  milliseconds
  """
  @spec time_left(Timex.Datetime, integer) :: integer
  defp time_left(_start_time, :infinity) do
    :inifinity
  end
  defp time_left(start_time, lease_time) do
    now = Timex.Date.now
    time_elapsed = Timex.Date.diff(now, start_time, :secs)
    case lease_time - time_elapsed do
      time when time <= 0 -> 0
      time -> time * 1000
    end
  end

  def handle_call(:fetch, _from, state = %State{value: value, lease_time: lease_time, start_time: start_time}) do
    timeout = time_left(start_time, lease_time)
    {:reply, {:ok, value}, state, timeout}
  end

  def handle_cast({:replace, value}, state = %State{lease_time: lease_time, start_time: start_time}) do
    timeout = time_left(start_time, lease_time)
    {:noreply, %State{ state | value: value }, timeout}
  end

  def handle_cast(:delete, state) do
    {:stop, :normal, state}
  end

  def handle_info(:timeout, state) do
    {:stop, :normal, state}
  end

  def terminate(_reason, _state) do
    SimpleCache.Store.delete(self)
    :ok
  end
end
