defmodule Drumbeat.URL do
  defstruct type: :http, url: nil
  def type(nil), do: :http
  def type("http"), do: :http
  def type("message_sink"), do: :message_sink
  def type(x) when is_atom(x), do: x

  def url("sender_pid"), do: :sender_pid

  def rewrite(x, x, y) when is_binary(x), do: y
  def rewrite(x, _, _) when is_binary(x), do: x
  def rewrite(from, from, to), do: to
  def rewrite(%Drumbeat.URL{url: from} = x, from, to) do
    %{x | url: to}
  end
  def rewrite(X, _From, _To), do: X
end

defimpl Poison.Decoder, for: Drumbeat.URL do
  def decode(value, _options) do
    value
    |> Map.put(:type, Drumbeat.URL.type(value.type))
    |> Map.put(:url, Drumbeat.URL.url(value.url))
  end
end
