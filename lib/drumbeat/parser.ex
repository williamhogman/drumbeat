alias Drumbeat.Request, as: Req
defmodule Drumbeat.Parser do
  @spec parse(Req.t) :: Req.t
  def parse(%Req{} = x) do
    Poison.Decoder.decode(x, nil)
    |> Req.rewrite_url(:sender_pid, self())
  end
  @spec parse([Req.t]) :: [Req.t]
  def parse(x) when is_list(x) do
    Req.rewrite_urls(x, :sender_pid, self())
  end
  @spec parse(binary) :: [Req.t]
  def parse(x) when is_binary(x) do
    x |> poison_parse_json |> parse
  end

  @spec parse(binary) :: Req.t | [Req.t]
  def parse_json(x) when is_binary(x) do
    x |> poison_parse_json |> parse
  end

  @spec parse_json(nil | binary) :: Req.t
  defp poison_parse_json(x), do: Poison.decode!(x, as: [Req], keys: :atoms)

  def with_template(fname, val) do
    fname |> from_file |> replace_template val
  end

  defp from_file(fname) do
    fname |> File.read! |> parse
  end

  defp replace_template(reqs, val) do
    reqs |> Enum.map fn
      %Req{type: :placeholder} -> val
      x -> x
    end
  end
end
