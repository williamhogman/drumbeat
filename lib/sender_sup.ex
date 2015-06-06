defmodule Drumbeat.SenderSup do
  use Supervisor

  # API
  def start_worker(sender_sup, uuid, request) do
    {:ok, _child} = Supervisor.start_child(sender_sup, [self(), uuid, request])
    :ok
  end

  def start_link do
    Supervisor.start_link(__MODULE__, :ok)
  end

  @worker_name Drumbeat.Sender

  def init(:ok) do
    children = [
      worker(@worker_name, [], restart: :transient)
    ]
    supervise(children,
              strategy: :simple_one_for_one,
              max_restarts: 5,
              max_seconds: 3600,
    )
  end
end
