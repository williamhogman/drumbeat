defmodule Drumbeat.DispatchSup do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok)
  end

  @dispatch_name Drumbeat.Dispatch
  @pool_name Drumbeat.SenderSup

  def start_sender_pool(sup_pid) do
    spec = worker(@pool_name, [])
    {:ok, sender_pool} = Supervisor.start_child(sup_pid, spec)
    sender_pool
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
