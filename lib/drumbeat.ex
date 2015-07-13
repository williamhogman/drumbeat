defmodule Drumbeat do
  use Application

  @spec start(any, any) :: {:ok, pid}
  def start(_type, _args) do
    {:ok, pid} = Drumbeat.DispatchSup.start_link()
    Drumbeat.Web.start
    {:ok, pid}
  end
  @spec start() :: {:ok, pid}
  def start, do: start(nil, nil)
end
