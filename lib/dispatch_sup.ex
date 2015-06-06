defmodule Drumbeat.DispatchSup do
  use Supervisor
  @worker_name Drumbeat.Sender
  @dispatch_name Drumbeat.Dispatch

  def start_link do
    Supervisor.start_link(__MODULE__, :ok)
  end


  def pool_spec, do: supervisor(Task.Supervisor,
                                [[max_restarts: 5,
                                 max_seconds: 3600,
                                 restart: :transient]])

  def start_sender_pool(sup_pid) do
    {:ok, pool} = Supervisor.start_child(sup_pid, pool_spec)
    pool
  end

  def start_request_worker(worker_sup, uuid, request) do
    {:ok, _child} = Task.Supervisor.start_child(
      worker_sup,
      @worker_name, :start_link,
      [self(), uuid, request])
    :ok
  end

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
