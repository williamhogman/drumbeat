defmodule Drumbeat.Request do
  defstruct url: nil, respond_to: nil, body: nil, headers: nil
  def successor(%Drumbeat.Request{respond_to: :end}, _headers, _body) do
    :end
  end
  def successor(%Drumbeat.Request{respond_to: respond_to}, headers, body) do
    Drumbeat.Request.from_template(respond_to)
    |> put_smart(:body, body)
    |> put_smart(:headers, headers)
  end

  def add_terminal_node(req, node) do
    case req.respond_to do
      nil -> Map.put(req, :respond_to, node)
      x -> Map.put(req, :respond_to, add_terminal_node(x, node))
    end
  end

  defp put_smart(map, _key, nil), do: map
  defp put_smart(map, key, value) do
    case Map.get(map, key) do
      nil -> Map.put(map, key, value)
      _x -> map
    end
  end

  def from_template(t) when is_map(t), do: t
  def message_sink(pid) do
    %Drumbeat.Request{
                 url: {:message_sink, pid},
                 respond_to: :end,
             }
  end
end

defimpl Poison.Decoder, for: Drumbeat.Request do
  def decode(value, _options) do
    Map.put(value, :respond_to, case value.respond_to do
                                  nil -> nil
                                  x -> Poison.Decoder.decode(value)
                                end)
  end
end
