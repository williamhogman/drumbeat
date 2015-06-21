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
  def place_request(pid, id, [_h|_t] = reqs), do: call(pid, {:place_request, id, reqs})

  def handle_call({:place_request, uuid, request}, _from, state) do
    {:reply, {:ok, uuid}, internal_place_request(state, uuid, request)}
  end

  def init([dispatch_sup]) do
    cast(self(), {:start_pool, dispatch_sup})
    registry = Drumbeat.Registry.new()
    {:ok, %Drumbeat.Dispatch{registry: registry}}
  end

  def handle_cast({:start_pool, pid}, state) do
    {:ok, pid} = Drumbeat.DispatchSup.start_sender_pool(pid)
    {:noreply, %Drumbeat.Dispatch{state | pool: pid}}
  end

  defp report_response(uuid, resp, state, task) do
    {[_|requests], new_reg} = Drumbeat.Registry.remove_request(state.registry, uuid)
    new_state = %Drumbeat.Dispatch{state | registry: new_reg}
    case next_req(requests, resp) do
      [] -> {:noreply, state}
      nil -> {:noreply, state}
      req -> {:noreply, internal_place_request(new_state, uuid, req, [task])}
    end
  end

  defp next_req([], resp) when is_map(resp), do: nil
  defp next_req([h|t], resp) when is_map(resp) do
    [Drumbeat.Request.successor(resp, h)|t]
  end
  defp next_req(reqs, resps) do
    resps ++ reqs
  end

  defp internal_place_request(state, uuid, [req|_] = reqs, remove_tasks \\ []) do
    new_reg = Drumbeat.Registry.place_request(state.registry, uuid, reqs)
    t = %Task{} = Drumbeat.DispatchSup.start_request_worker(state.pool, uuid, req)
    %Drumbeat.Dispatch{state|tasks: [t|state.tasks] -- remove_tasks, registry: new_reg}
  end

  def handle_info(msg, state) do
    case Task.find(state.tasks, msg) do
      nil -> {:noreply, state}
      {{id, r}, task} -> report_response(id, r, state, task)
    end
  end
end
