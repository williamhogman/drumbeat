defmodule Drumbeat.SenderSup do
  import Task.Supervisor
  @worker_name Drumbeat.Sender

  # API
  def start_worker(pid, uuid, request) do
    {:ok, _child} = start_child(pid,
                                @worker_name, :start_link,
                                [self(), uuid, request])
    :ok
  end

  def start_link, do: start_link(max_restarts: 5, max_seconds: 3600, restart: transient)
end
