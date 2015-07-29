defmodule Drumbeat.Request do
  alias Drumbeat.Request
  defstruct url: nil, body: nil, headers: nil, method: nil, type: :http
  @type reqPart :: :url | :body | :headers | :method | :type
  @type t :: %Request{}

  @spec successor(t, t) :: t
  def successor(%Request{} = resp, %Request{} = next) do
    next
    |> put_smart(:body, resp.body)
    |> put_smart(:headers, resp.headers)
    |> put_smart(:method, resp.method)
    |> put_smart(:url, resp.url)
    |> put_smart(:type, resp.type)
  end

  @spec rewrite_url(t, any, any) :: t
  def rewrite_url(req, from, to) do
    %{req | url: rewrite(req.url, from, to)}
  end

  @spec rewrite_urls([t], any, any) :: [t]
  def rewrite_urls([], _, _), do: []
  def rewrite_urls([h|t], from, to) do
    [rewrite_url(h, from, to)|rewrite_urls(t, from, to)]
  end
  defp rewrite(from, from, to), do: to
  defp rewrite(x, _from, _to), do: x

  @spec put_smart(t, reqPart, any) :: t
  defp put_smart(map, _key, nil), do: map
  defp put_smart(map, key, value) do
    case Map.get(map, key) do
      nil -> Map.put(map, key, value)
      _x -> map
    end
  end

  @spec message_sink(pid) :: t
  def message_sink(pid) do
    %Request{type: :message_sink, url: pid}
  end
  @spec message_sink() :: t
  def message_sink, do: message_sink(self())
  @spec quote_req() :: t
  def quote_req, do: %Request{type: :quote}
end

defimpl Poison.Decoder, for: Drumbeat.Request do
  @typep types :: :http | :message_sink | :quote | :placeholder | :eval
  @spec type(binary | atom | binary) :: types
  defp type(nil), do: :http
  defp type("http"), do: :http
  defp type("message_sink"), do: :message_sink
  defp type("quote"), do: :quote
  defp type("placeholder"), do: :placeholder
  defp type("eval"), do: :eval
  defp type("http_hook"), do: :http_hook
  defp type(x) when is_atom(x), do: x

  @spec url(binary) :: binary | :sender_pid
  def url("sender_pid"), do: :sender_pid
  def url(x), do: x

  @typep http_methods :: :get | :head | :post | :put | :delete | :patch
  @spec method_table(binary) :: nil | http_methods
  defp method_table(""), do: nil
  defp method_table("get"), do: :get
  defp method_table("head"), do: :head
  defp method_table("post"), do: :post
  defp method_table("put"), do: :put
  defp method_table("delete"), do: :delete
  defp method_table("patch"), do: :patch

  @spec method(nil | binary | Atom) :: Atom
  defp method(nil), do: nil
  defp method(x) when is_binary(x), do: method_table(String.downcase(x))
  defp method(x) when is_atom(x), do: x


  def decode(nil, _opts), do: nil
  def decode(value, _opts) do
    value
    |> Map.put(:method, method(value.method))
    |> Map.put(:type, type(value.type))
    |> Map.put(:url, url(value.url))
  end
end

defimpl Poison.Encoder, for: PID do
  def encode(value, opts) do
    Poison.Encoder.encode(:erlang.pid_to_list(value), opts)
  end
end
