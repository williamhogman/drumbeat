defmodule Drumbeat do
  use Application
  import Supervisor.Spec

  @spec start(any, any) :: {:ok, pid}
  def start(_type, _args) do
    Supervisor.start_link(__MODULE__, nil)
  end
  @spec start() :: {:ok, pid}
  def start, do: start(nil, nil)

  def init(arg) do
    supervise(children, strategy: :one_for_one)
  end

  @web_proc Drumbeat.Web
  @dispatch_sup_proc Drumbeat.DispatchSup
  @hooks_store_proc Drumbeat.HooksStore
  defp children, do: [
    worker(@web_proc, [], restart: :transient),
    worker(@dispatch_sup_proc, [], restart: :transient),
    worker(@hooks_store_proc, [[name: @hooks_store_proc]], restart: :transient)
  ]
end
