defmodule Drumbeat.Request do
  defstruct url: nil, body: nil, headers: nil, method: nil, type: :http
  def successor(%Drumbeat.Request{} = resp, %Drumbeat.Request{} = next) do
    next
    |> put_smart(:body, resp.body)
    |> put_smart(:headers, resp.headers)
  end

  def rewrite_url(req, from, to) do
    %{req | url: rewrite(req.url, from, to)}
  end
  def rewrite_urls([], _, _), do: []
  def rewrite_urls([h|t], from, to) do
    [rewrite_url(h, from, to)|rewrite_urls(t, from, to)]
  end
  defp rewrite(from, from, to), do: to
  defp rewrite(x, _from, _to), do: x

  defp put_smart(map, _key, nil), do: map
  defp put_smart(map, key, value) do
    case Map.get(map, key) do
      nil -> Map.put(map, key, value)
      _x -> map
    end
  end

  def message_sink(pid) do
    %Drumbeat.Request{type: :message_sink, url: pid}
  end
  def message_sink, do: message_sink(self())
  def quote_req, do: %Drumbeat.Request{type: :quote}
end

defimpl Poison.Decoder, for: Drumbeat.Request do
  defp type(nil), do: :http
  defp type("http"), do: :http
  defp type("message_sink"), do: :message_sink
  defp type("quote"), do: :quote
  defp type("placeholder"), do: :placeholder
  defp type("eval"), do: :eval
  defp type(x) when is_atom(x), do: x

  def url("sender_pid"), do: :sender_pid
  def url(x), do: x

  defp method(nil), do: nil
  defp method(""), do: nil
  defp method("get"), do: :get
  defp method("head"), do: :head
  defp method("post"), do: :post
  defp method("put"), do: :put
  defp method(x) when is_binary(x), do: String.downcase(x)
  defp method(x) when is_atom(x), do: x

  def decode(nil, opts), do: nil
  def decode(value, opts) do
    value
    |> Map.put(:method, method(value.method))
    |> Map.put(:type, type(value.type))
    |> Map.put(:url, url(value.url))
  end
end

defimpl Poison.Encoder, for: PID do
  def encode(value, opts) do
    Poison.Encoder.encode("pid!", opts)
  end
end
