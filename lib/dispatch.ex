defmodule Drumbeat.Dispatch do
  import GenServer
  use GenServer
  @type t :: %Drumbeat.Dispatch{tasks: [Task.t], pool: nil | pid}
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

  @spec init([PID]) :: {:ok, any}
  def init([dispatch_sup]) do
    cast(self(), {:start_pool, dispatch_sup})
    registry = Drumbeat.Registry.new()
    {:ok, %Drumbeat.Dispatch{registry: registry}}
  end

  def handle_cast({:start_pool, pid}, state) do
    {:ok, pid} = Drumbeat.DispatchSup.start_sender_pool(pid)
    {:noreply, %Drumbeat.Dispatch{state | pool: pid}}
  end

  @spec report_response(binary, Drumbeat.Request.t, Drumbeat.Dispatch.t, Task.t) :: any
  defp report_response(uuid, resp, state, task) do
    adv = Drumbeat.Registry.advance_request(state.registry, uuid, resp)
    case adv do
      {nil, new_reg} ->
        %Drumbeat.Dispatch{state | registry: new_reg}
      {reqs, new_reg} ->
        internal_place_request(%Drumbeat.Dispatch{state | registry: new_reg}, uuid, reqs, [task])
    end
  end

  @spec internal_place_request(t, binary, [Req.t], [Task.t]) :: t
  defp internal_place_request(state, uuid, [req|_] = reqs, remove_tasks \\ []) do
    new_reg = Drumbeat.Registry.place_request(state.registry, uuid, reqs)
    t = %Task{} = Drumbeat.DispatchSup.start_request_worker(state.pool, uuid, req)
    %Drumbeat.Dispatch{state|tasks: [t|state.tasks] -- remove_tasks, registry: new_reg}
  end

  def handle_info(msg, state) do
    case Task.find(state.tasks, msg) do
      nil -> {:noreply, state}
      {{id, r}, task} -> {:noreply, report_response(id, r, state, task)}
    end
  end
end
