# It handles all the pipes.

defmodule Hermit.Plumber do
  require Logger

  defmodule Pipe do
    defstruct id: '', fp: nil, active: false, listeners: []
  end

  def start_link do
    Task.async(&Hermit.Plumber.reap_loop/0)
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  # Create a new pipe and return the id
  def new_pipe do
    Agent.get_and_update(__MODULE__, fn state ->
      # Generate a random id to use as the file name
      pipe_id = :crypto.strong_rand_bytes(6)
      |> Base.url_encode64

      # TODO: make log directory configurable
      {:ok, file} =
        pipe_id
        |> get_pipe_file()
        |> File.open([:raw, :write])

      next_state = Map.put(state, pipe_id,
        %Pipe{id: pipe_id, fp: file, active: true})

      { pipe_id, next_state }
    end)
  end

  def add_pipe_listener(pipe_id, pid) do
    Agent.update(__MODULE__, fn state ->
      Map.update(state, pipe_id, %Pipe{id: pipe_id}, fn pipe ->
        %{pipe | listeners: [pid | pipe.listeners]}
      end)
    end)
  end

  # Send a message to all PIDs listening to this pipe
  defp broadcast_pipe_listeners(pipe, msg) do
    pipe
    |> Map.get(:listeners)
    |> Enum.each(fn pid ->
      IO.puts "sending to #{inspect pid}"
      send(pid, msg)
    end)
  end

  # FIXME: need a limit on total bytes written.
  def pipe_input(pipe_id, content) do
    Agent.get(__MODULE__, fn state ->
      pipe = Map.get(state, pipe_id, %Pipe{})
      true = pipe.active
      :ok = IO.binwrite(pipe.fp, content)

      pipe
    end)
    |> broadcast_pipe_listeners({ :pipe_activity, content })
  end

  def close_pipe(pipe_id) do
    Agent.get_and_update(__MODULE__, fn state ->
      state_ = Map.update!(state, pipe_id, fn pipe ->
        :ok = File.close(pipe.fp)
        %{ pipe | active: false }
      end)

      { Map.get(state_, pipe_id), state_ }
    end)
    |> broadcast_pipe_listeners({ :closed })
  end

  def get_pipe_file(pipe_id) do
    # TODO: need to make this configurable.
    Path.join("/tmp/hermit/", pipe_id)
  end

  # Clean up the dead procs every 60 seconds
  def reap_loop do
    Process.sleep(60_000)

    Agent.update(__MODULE__, fn state ->
      state
      |> Enum.map(fn {id, pipe} ->
        {id, %{ pipe | listeners: Enum.filter(pipe.listeners, &Process.alive?/1)}}
      end)
      |> Enum.into(%{})
    end)

    reap_loop()
  end
end
