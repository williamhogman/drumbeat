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
  Advances a request list one step.
  """
  def advance_request(reg, uuid, resp) do
    [_|requests] = Dict.get(reg, uuid, nil)
    case next_req(requests, resp) do
      [] -> {nil, Dict.delete(reg, uuid)}
      nil -> {nil, Dict.delete(reg, uuid)}
      req -> {req, Dict.delete(reg, uuid)}
    end
  end
  defp next_req(nil, resp) when is_map(resp), do: nil
  defp next_req([], resp) when is_map(resp), do: nil
  defp next_req([h|t], resp) when is_map(resp) do
    [Drumbeat.Request.successor(resp, h)|t]
  end
  defp next_req(reqs, resps) do
    resps ++ reqs
  end
end
