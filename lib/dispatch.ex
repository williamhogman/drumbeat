defmodule Drumbeat.Dispatch do
  import GenServer
  use GenServer
  defstruct registry: nil, pool: nil, tasks: []

  @doc """
  Starts the request dispatcher
  """
  def start_link(dispatch_sup, opts \\ []) do
    GenServer.start_link(__MODULE__, [dispatch_sup], opts)
  end

  @doc """
  Places an HTTP request
  """
  def place_request(pid, uuid, request), do: call(pid, {:place_request, uuid, request})
  def report_response(pid, uuid, resp), do: cast(pid, {:report_response, uuid, resp})

  def get_status(pid, uuid), do: call(pid, {:get_status, uuid})

  def stop(pid), do: call(pid, :stop)

  def init([dispatch_sup]) do
    cast(self(), {:start_pool, dispatch_sup})
    {:ok, registry} = Drumbeat.Registry.start_link()
    {:ok, %Drumbeat.Dispatch{registry: registry}}
  end

  def handle_cast({:start_pool, pid}, state) do
    pool = Drumbeat.DispatchSup.start_sender_pool(pid)
    {:noreply, %Drumbeat.Dispatch{state | pool: pool}}
  end

  def handle_cast({:report_response, uuid, resp}, state) do
    {:ok, requests} = Drumbeat.Registry.remove_request(state.registry, uuid)
    new_state = case requests do
      [_h|[]] -> state
      [_|[next|t]] ->
        successor = Drumbeat.Request.successor(resp, next)
        internal_place_request(state, uuid, [successor|t])
    end
    {:noreply, new_state}
  end

  defp internal_place_request(state, uuid, [req|_] = reqs) do
    :ok = Drumbeat.Registry.place_request(state.registry, uuid, reqs)
    t = %Task{} = Drumbeat.DispatchSup.start_request_worker(state.pool, uuid, req)
    %Drumbeat.Dispatch{state|tasks: [t|state.tasks]}
  end

  def handle_call({:place_request, uuid, request},
                  _from, state) do
    new_state = internal_place_request(state, uuid, request)
    {:reply, {:ok, uuid}, new_state}
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
    case Task.find(state.tasks, msg) do
      {{id, %Drumbeat.Request{} = req}, _ref} ->
        report_response(self(), id, req)
      nil -> nil
    end
    {:noreply, state}
  end
end
