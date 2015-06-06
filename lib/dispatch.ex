defmodule Drumbeat.Dispatch do
  use GenServer
  defstruct registry: nil, pool: nil, requests: nil

  @doc """
  Starts the request dispatcher
  """
  def start_link(dispatch_sup, opts \\ []) do
    GenServer.start_link(__MODULE__, [dispatch_sup], opts)
  end

  @doc """
  Places an HTTP request
  """
  def place_request(dispatch, uuid, request) do
    GenServer.call(dispatch, {:place_request, uuid, request})
  end

  def report_response(dispatch, uuid, resp) do
    GenServer.cast(dispatch, {:report_response, uuid, resp})
    :ok
  end

  def get_status(dispatch, uuid) do
    GenServer.call(dispatch, {:get_status, uuid})
  end

  def stop(server), do: GenServer.call(server, :stop)

  def init([dispatch_sup]) do
    GenServer.cast(self(), {:start_pool, dispatch_sup})
    {:ok, registry} = Drumbeat.Registry.start_link()
    {:ok, %Drumbeat.Dispatch{registry: registry}}
  end

  def handle_cast({:start_pool, pid}, state) do
    pool = Drumbeat.DispatchSup.start_sender_pool(pid)
    {:noreply, %Drumbeat.Dispatch{state | pool: pool}}
  end

  def handle_cast({:report_response, uuid, resp}, state) do
    {:ok, requests} = Drumbeat.Registry.remove_request(state.registry, uuid)
    case requests do
      [_h|[]] -> :ok
      [_|[next|t]] ->
        successor = Drumbeat.Request.successor(resp, next)
        internal_place_request(state, uuid, [successor|t])
    end
    {:noreply, state}
  end

  defp internal_place_request(state, uuid, [req|_] = reqs) do
    registry = state.registry
    pool = state.pool
    :ok = Drumbeat.Registry.place_request(registry, uuid, reqs)
    %Task{} = Drumbeat.DispatchSup.start_request_worker(pool, uuid, req)
  end


  def handle_call({:place_request, uuid, request},
                  _from, state) do
    internal_place_request(state, uuid, request)
    {:reply, {:ok, uuid}, state}
  end

  def handle_call({:get_status, uuid}, _from, state) do
    registry = state.registry
    status = Drumbeat.Registry.has_request(registry, uuid)
    {:reply, {:ok, status}, state}
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, :ok, state}
  end

  def handle_info(msg, state) do
    case msg do
      {ref, {id, %Drumbeat.Request{} = req}} when is_reference(ref) ->
        report_response(self(), id, req)
      _ -> nil
    end

    {:noreply, state}
  end
end
