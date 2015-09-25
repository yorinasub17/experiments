defmodule LoadTestFtp.Worker do
  use GenServer

  defmodule State do
    defstruct conn: nil
  end

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def upload(path) do
    GenServer.cast(__MODULE__, {:upload, path})
  end

  def upload(path, n) when n <= 1 do
    upload(path)
  end
  def upload(path, n) do
    upload(path)
    upload(path, n-1)
  end


  def init(:ok) do
    {:ok, conn} = :ssh.connect('ec2-54-209-190-175.compute-1.amazonaws.com', 22, [{:user, 'ubuntu'}, {:silently_accept_hosts, true}, {:user_interaction, false}, {:user_dir, '/Users/yoriy/.elixir_connection'}])
    {:ok, %State{conn: conn}}
  end

  def handle_cast({:upload, path}, state = %State{conn: conn}) do
    spawn fn -> upload_file(path, conn) end
    {:noreply, state}
  end

  def terminate(_reason, %State{conn: conn}) do
    :ssh.close(conn)
  end

  def upload_file(path, conn) do
    :random.seed(:erlang.now)
    new_number = :random.uniform()
    {:ok, channel} = :ssh_sftp.start_channel(conn, [])
    :ok = :ssh_sftp.write_file(channel, "/home/ubuntu/test/" <> Float.to_string(new_number), 'foozle')
    :ok = :ssh_sftp.stop_channel(channel)
  end
end
