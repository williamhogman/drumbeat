defmodule Drumbeat.Request do
  defstruct url: nil, body: nil, headers: nil, method: nil, type: :http

  @type req :: %Drumbeat.Request{}

  @spec successor(req, req) :: req
  def successor(%Drumbeat.Request{} = resp, %Drumbeat.Request{} = next) do
    next
    |> put_smart(:body, resp.body)
    |> put_smart(:headers, resp.headers)
    |> put_smart(:method, resp.method)
    |> put_smart(:url, resp.url)
    |> put_smart(:type, resp.type)
  end

  @spec rewrite_url(req, any, any) :: req
  def rewrite_url(req, from, to) do
    %{req | url: rewrite(req.url, from, to)}
  end

  @spec rewrite_urls([req], any, any) :: [req]
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

  @spec message_sink(pid) :: req
  def message_sink(pid) do
    %Drumbeat.Request{type: :message_sink, url: pid}
  end
  @spec message_sink() :: req
  def message_sink, do: message_sink(self())
  @spec quote_req() :: req
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


  defp method_table(""), do: nil
  defp method_table("get"), do: :get
  defp method_table("head"), do: :head
  defp method_table("post"), do: :post
  defp method_table("put"), do: :put
  defp method_table("delete"), do: :delete
  defp method_table("patch"), do: :patch

  defp method(nil), do: nil
  defp method(x) when is_binary(x), do: method_table(String.downcase(x))
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
