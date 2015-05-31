alias Drumbeat.Request, as: Req
defmodule Drumbeat.Parser do
  defp parse_json(nil), do: nil
  defp parse_json(data), do: Poison.decode!(data, as: Req)
  def parse(body) do
    body
    |> parse_json
    |> Drumbeat.Request.rewrite_urls(:sender_pid, self())
  end

  def parse_and_decorate(body) do
    body
    |> parse
    |> Req.add_terminal_node(Req.quote_req)
    |> Req.add_terminal_node(Req.message_sink)
  end
end