alias Drumbeat.Request, as: Req
defmodule Drumbeat.Parser do
  defp parse_json(nil), do: nil
  defp parse_json(x), do: Poison.decode!(x, as: [Drumbeat.Request], keys: :atoms)
  def parse(%Drumbeat.Request{} = x) do
    Poison.Decoder.decode(x, nil)
    |> Drumbeat.Request.rewrite_url(:sender_pid, self())
  end
  def parse(x) when is_list(x) do
    Drumbeat.Request.rewrite_urls(x, :sender_pid, self())
  end
  def parse(x) when is_binary(x) do
    x |> parse_json |> parse
  end

  def parse_and_decorate(body) do
    parse(body) ++ [Req.quote_req, Req.message_sink]
  end

  def with_template(fname, val) do
    fname |> from_file |> replace_template val
  end

  defp from_file(fname) do
    fname |> File.read! |> parse
  end

  defp replace_template(reqs, val) do
    reqs |> Enum.map fn
      %Drumbeat.Request{type: :placeholder} -> val
      x -> x
    end
  end
end
