defmodule Drumbeat.HooksStore do
  def start_link(opts \\ []) do
    Agent.start_link(__MODULE__, :init, [], opts)
  end

  def init, do: HashDict.new

  def put(pid, {_s, _n, _k} = full_key, value) do
    Agent.update(pid, &Dict.put(&1, full_key, value))
  end
  def put(fk, v), do: put(__MODULE__, fk, v)

  def get(pid, {_s, _n, _k} = full_key) do
    Agent.get(pid, &Dict.get(&1, full_key))
  end
  def get(fk), do: get(__MODULE__, fk)
end
