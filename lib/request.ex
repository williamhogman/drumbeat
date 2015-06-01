defmodule Drumbeat.Request do
  defstruct url: nil, respond_to: nil, body: nil, headers: nil, method: nil
  def successor(%Drumbeat.Request{respond_to: nil}, _, _), do: nil
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
    new = Map.put(req, :respond_to, child)
    f.(new)
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

  defp decode_respond_to(nil, _), do: nil
  defp decode_respond_to(value, options) do
    struct = Drumbeat.Request.__struct__
    Enum.into(Map.from_struct(struct), %{}, fn {key, default} ->
      {key, Map.get(value, Atom.to_string(key), default)}
    end)
    |> Map.put(:__struct__, struct.__struct__)
    |> decode(options)
  end

  def decode(nil, opts), do: nil
  def decode(value, opts) do
    value
    |> Map.put(:respond_to, decode_respond_to(value.respond_to, opts))
    |> Map.put(:url, decode_url(value.url, opts))
    |> Map.put(:method, decode_method(value.method))
  end
end
