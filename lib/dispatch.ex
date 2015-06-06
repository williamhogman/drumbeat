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

  def handle_call({:place_request, uuid, request}, _from, state) do
    {:reply, {:ok, uuid}, internal_place_request(state, uuid, request)}
  end

  def init([dispatch_sup]) do
    cast(self(), {:start_pool, dispatch_sup})
    {:ok, registry} = Drumbeat.Registry.start_link()
    {:ok, %Drumbeat.Dispatch{registry: registry}}
  end

  def handle_cast({:start_pool, pid}, state) do
    pool = Drumbeat.DispatchSup.start_sender_pool(pid)
    {:noreply, %Drumbeat.Dispatch{state | pool: pool}}
  end

  defp report_response(uuid, resp, state, task) do
    {:ok, [_|requests]} = Drumbeat.Registry.remove_request(state.registry, uuid)
    case next_req(requests, resp) do
      nil -> {:noreply, resp}
      req -> {:noreply, internal_place_request(state, uuid, req, [task])}
    end
  end

  defp next_req([], _), do: nil
  defp next_req([next|t], resp),  do: [Drumbeat.Request.successor(resp, next)|t]

  defp internal_place_request(state, uuid, [req|_] = reqs, remove_tasks \\ []) do
    :ok = Drumbeat.Registry.place_request(state.registry, uuid, reqs)
    t = %Task{} = Drumbeat.DispatchSup.start_request_worker(state.pool, uuid, req)
    %Drumbeat.Dispatch{state|tasks: [t|state.tasks] -- remove_tasks}
  end

  def handle_info(msg, state) do
    case Task.find(state.tasks, msg) do
      nil -> {:noreply, state}
      {{id, %Drumbeat.Request{} = r}, task} -> report_response(id, r, state, task)
    end
  end
end
