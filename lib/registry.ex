defmodule Drumbeat.Registry do
  @doc """
  Create a new registry
  """
  @spec new() :: %{}
  def new(), do: %{}

  @doc """
  Place a request to the registry
  """
  def place_request(reg, id, req) do
    Dict.put(reg, id, req)
  end

  @doc """
  Removes a request from the registry
  """
  def remove_request(reg, id) do
    Dict.pop(reg, id)
  end
end
