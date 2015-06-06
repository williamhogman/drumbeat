defmodule Drumbeat.Request do
  defstruct url: nil, body: nil, headers: nil, method: nil
  def successor(%Drumbeat.Request{} = current, %Drumbeat.Request{} = next) do
    Drumbeat.Request.from_template(next)
    |> put_smart(:body, current.body)
    |> put_smart(:headers, current.headers)
  end

  def rewrite_url(req, from, to) do
    %{req | url: Drumbeat.URL.rewrite(req.url, from, to)}
  end
  def rewrite_urls([], _, _), do: []
  def rewrite_urls([h|t], from, to) do
    [rewrite_url(h, from, to)|rewrite_urls(t, from, to)]
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
    %Drumbeat.Request{url: %Drumbeat.URL{type: :message_sink, url: pid}}
  end
  def message_sink do
    message_sink(self())
  end
  def quote_req do
    %Drumbeat.Request{url: %Drumbeat.URL{type: :quote}}
  end
end

defimpl Poison.Decoder, for: Drumbeat.Request do
  defp decode_url(x) when is_map(x) do
    Poison.Decoder.decode(x, as: Drumbeat.URL, keys: :atoms!)
  end

  defp decode_url(x, _opts) when is_binary(x) or is_list(x) do
    Drumbeat.URL.from_text(x)
  end
  defp decode_url(nil, _options), do: %Drumbeat.URL{}
  defp decode_url(value, options) do
    struct = Drumbeat.URL.__struct__
    Enum.into(Map.from_struct(struct), %{}, fn {key, default} ->
      {key, Map.get(value, Atom.to_string(key), default)}
    end)
    |> Map.put(:__struct__, struct.__struct__)
    |> decode_url(options)
  end

  def decode_method(nil), do: nil
  def decode_method(""), do: nil
  def decode_method("get"), do: :get
  def decode_method("head"), do: :head
  def decode_method("post"), do: :post
  def decode_method("put"), do: :put
  def decode_method(x) when is_binary(x), do: String.downcase(x)

  def decode(nil, opts), do: nil
  def decode(value, opts) do
    value
    |> Map.put(:url, decode_url(value.url, opts))
    |> Map.put(:method, decode_method(value.method))
  end
end
