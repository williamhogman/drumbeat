defmodule Drumbeat.Registry do
  @doc """
  Starts a new registry
  """
  def start_link do
    Agent.start_link(fn -> %{} end)
  end

  @doc """
  Returns true IFF the specified request is presently
  in the registry.
  """
  def has_request(registry, id) do
    Agent.get(registry, &Dict.has_key?(&1, id))
  end

  @doc """
  Place a request to the registry
  """
  def place_request(registry, id, request) do
    Agent.update(registry, &Dict.put(&1, id, request))
  end

  @doc """
  Removes a request from the registry
  """
  def remove_request(registry, id) do
    case Agent.update(registry, &Dict.pop(&1, id)) do
      nil -> :err
      value -> {:ok, value}
    end
  end
end
