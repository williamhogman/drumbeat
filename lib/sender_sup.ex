defmodule Drumbeat.SenderSup do
  use Supervisor

  # API
  def start_worker(sender_sup, request) do
    Supervisor.start_child(sender_sup, [self(), request])
  end

  def start_link do
    Supervisor.start_link(__MODULE__, :ok)
  end

  @worker_name Drumbeat.Sender

  def init(:ok) do
    children = [
      worker(@worker_name, [], restart: :temporary)
    ]
    supervise(children,
              strategy: :simple_one_for_one,
              max_restarts: 5,
              max_seconds: 3600,
    )
  end
end
