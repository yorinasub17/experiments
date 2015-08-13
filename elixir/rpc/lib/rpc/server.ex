defmodule RPC.Server do
  @moduledoc """
  RPC over TCP server. This module defines a server process that listens for
  incoming TCP connections and allows the user to execute RPC commands via that
  TCP stream.
  """

  @server __MODULE__
  @default_port 1055

  use GenServer

  defmodule State do
    defstruct port: 0, lsock: nil, request_count: 0
  end
  @type state :: %State{port: integer, lsock: pid, request_count: integer}

  ### API
  @doc """
  Starts the server
  """
  @spec start_link(integer) :: {:ok, pid}
  def start_link(port) do
    GenServer.start_link(__MODULE__, port, name: @server)
  end
  
  @doc """
  Calls `start_link(port)` with the default port.
  """
  @spec start_link() :: {:ok, pid}
  def start_link() do
    start_link(@default_port)
  end

  @doc """
  Fetches the number of requests made to this server.
  """
  @spec get_count() :: {:ok, integer}
  def get_count() do
    GenServer.call(@server, :get_count)
  end

  @doc """
  Terminates the running server.
  """
  @spec stop() :: :ok
  def stop() do
    GenServer.cast(@server, :stop)
  end

  ### Internal GenServer implementation
  def init(port) do
    {:ok, lsock} = :gen_tcp.listen(port, [active: true])
    {:ok, %State{port: port, lsock: lsock}, 0}
  end

  def handle_call(:get_count, _from, state) do
    {:reply, {:ok, state.request_count}, state}
  end

  def handle_cast(:stop, state) do
    {:stop, :normal, state}
  end

  def handle_info({:tcp, socket, rawdata}, state = %State{request_count: request_count}) do
    IO.inspect(rawdata)
    do_rpc(socket, rawdata)
    {:noreply, %State{ state | request_count: request_count+1 }}
  end
  def handle_info(:timeout, state = %State{lsock: lsock}) do
    {:ok, _sock} = :gen_tcp.accept(lsock)
    {:noreply, state}
  end

  @doc """
  rawdata is a string of the form: "module:function(arglist)"
  """
  defp do_rpc(socket, rawdata) do
    {module, function, args} = extract_mfa(rawdata)
    result = apply(module, function, args)
    :gen_tcp.send(socket, "#{inspect(result)}\n")
  end

  defp extract_mfa(rawdata) do
    [_match, module, function, arguments] = Regex.run(~r/([^:]+):([^()]+)\(([^)]*)\)/, to_string(rawdata))
    {module, _bindings} = Code.eval_string(module)
    {module, String.to_atom(function), parse_args(arguments)}
  end

  defp parse_args(arguments) do
    String.split(arguments, ",")
    |> Enum.map(&String.strip/1)
    |> Enum.map(&Code.eval_string/1)
    |> Enum.map(&(elem(&1, 0)))
  end

  def test(n1, n2) do
    {n1, n2}
  end
end
