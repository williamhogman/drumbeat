defmodule Drumbeat.Dispatch do
  use GenServer
  require Record
  Record.defrecord :state, registry: nil, pool: nil

  @doc """
  Starts the request dispatcher
  """
  def start_link(dispatch_sup) do
    GenServer.start_link(__MODULE__, dispatch_sup, [])
  end

  @doc """
  Places an HTTP request
  """
  def place_request(dispatch, uuid, request) do
    GenServer.call(dispatch, {:place_request, uuid, request})
  end

  def place_request(dispatch, request) do
    place_request(dispatch, nil, request)
  end

  def report_response(dispatch, data) do
    GenServer.cast(dispatch, {:report_response, data})
    :ok
  end

  def get_status(dispatch, uuid) do
    GenServer.call(dispatch, {:get_status, uuid})
  end

  def stop(server) do
    GenServer.call(server, :stop)
  end

  def init(dispatch_sup) do
    GenServer.cast(self(), {:start_pool, dispatch_sup})
    {:ok, registry} = Drumbeat.Registry.start_link()
    {:ok, state(registry: registry)}
  end

  def handle_cast({:start_pool, pid}, current_state) do
    pool = Drumbeat.DispatchSup.start_sender_pool(pid)
    {:noreply, state(current_state, pool: pool)}
  end

  def handle_call({:request, uuid, request},
      _from, {registry}) do
    :ok = Drumbeat.Registry.place_request(registry, uuid, request)
    {:reply, {:ok, uuid}}
  end

  def handle_call({:get_status, uuid}, _from, {registry}) do
    status = Drumbeat.Registry.has_request(registry, uuid)
    {:reply, {:ok, status}}
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end
end
