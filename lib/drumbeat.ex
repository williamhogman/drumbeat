defmodule Drumbeat do
  use Application
  def start(_type, _args) do
    {:ok, pid} = Drumbeat.DispatchSup.start_link()
    Drumbeat.Web.start
    {:ok, pid}
  end
end
