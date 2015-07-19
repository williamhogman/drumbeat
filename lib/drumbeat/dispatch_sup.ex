defmodule Drumbeat.DispatchSup do
  use Supervisor
  @worker_name Drumbeat.Sender
  @dispatch_name Drumbeat.Dispatch

  @spec start_link() :: Supervisor.on_start
  def start_link, do: Supervisor.start_link(__MODULE__, :ok, [])

  defp pool_spec, do: supervisor(
            Task.Supervisor,
            [[max_restarts: 5, max_seconds: 3600,
              restart: :transient]])


  def start_sender_pool(sup_pid), do: Supervisor.start_child(sup_pid, pool_spec)

  @spec start_request_worker(pid, binary, Drumbeat.Request.t) :: Task.t
  def start_request_worker(worker_sup, id, req), do: Task.Supervisor.async(
            worker_sup,
            @worker_name, :run,
            [id, req])

  def init(:ok) do
    children = [
      worker(@dispatch_name, [self(), [name: @dispatch_name]])
    ]
    supervise(children,
              strategy: :one_for_all,
              max_restarts: 1,
              max_seconds: 3600
    )
  end
end
