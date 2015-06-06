defmodule Drumbeat.URL do
  defstruct type: :http, url: nil
  def type(nil), do: :http
  def type("http"), do: :http
  def type("message_sink"), do: :message_sink
  def type("quote"), do: :quote
  def type(x) when is_atom(x), do: x


  def url("sender_pid"), do: :sender_pid
  def url(x), do: x

  def rewrite(%Drumbeat.URL{url: from} = x, from, to), do: %{x | url: to}
  def rewrite(from, from, to), do: to
  def rewrite(x, _from, _to), do: x

  def from_text(x), do: %Drumbeat.URL{url: url(x), type: type(nil)}
end

defimpl Poison.Decoder, for: Drumbeat.URL do
  def decode(value, _options) do
    value
    |> Map.put(:type, Drumbeat.URL.type(value.type))
    |> Map.put(:url, Drumbeat.URL.url(value.url))
  end
end

defimpl Poison.Encoder, for: PID do
  def encode(value, opts) do
    Poison.Encoder.encode("pid!", opts)
  end
end
