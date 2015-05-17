defmodule Drumbeat.Request do
  defstruct url: nil, respond_to: nil, body: nil, headers: nil
  def successor(%Drumbeat.Request{respond_to: :end}, headers, body) do
    :end
  end
  def successor(%Drumbeat.Request{respond_to: respond_to}, headers, body) do
    Drumbeat.Request.from_template(respond_to)
    |> put_smart(:body, body)
    |> put_smart(:headers, headers)
  end

  def add_terminal_node(req, node) do
    visit_requests(req, fn
    (nil) -> node
    (x) -> x
    end)
  end

  def rewrite_urls(req, from, to) do
    visit_requests(req, fn
    (nil) -> nil
    (%Drumbeat.Request{} = req) ->
        %{req | url: Drumbeat.URL.rewrite(req.url, from, to)}
    (x) -> x
    end)

  end

  def visit_requests(nil, f), do: f.(nil)
  def visit_requests(req, f) when is_map(req) and is_function(f) do
    respond_to = req.respond_to
    child = visit_requests(respond_to, f)
    IO.inspect(child)
    new = Map.put(req, :respond_to, child)
    f.(new)
  end
  def visit_request(x, y) do
    IO.inspect(x)
    IO.inspect(y)
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
  defp decode_respond_to(nil), do: nil
  defp decode_respond_to(x) do
    Poison.Decoder.decode(x, as: Drumbeat.Request)
  end

  defp decode_url(x) when is_map(x) do
    Poison.Decoder.decode(x, as: Drumbeat.URL)
  end
  defp decode_url(x), do: x

  def decode(value, _options) do
    value
    |> Map.put(:respond_to, decode_respond_to(value.respond_to))
    |> Map.put(:url, decode_url(value.url))
  end
end
