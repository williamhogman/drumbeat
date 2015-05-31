defmodule Drumbeat.Parser do
  defp parse_json(nil), do: nil
  defp parse_json(data), do: Poison.decode!(data, as: Drumbeat.Request)
  def parse(body) do
    body
    |> parse_json
    |> Drumbeat.Request.rewrite_urls(:sender_pid, self())
    |> Drumbeat.Request.add_terminal_node(Drumbeat.Request.quote_req)
    |> Drumbeat.Request.add_terminal_node(Drumbeat.Request.message_sink)
  end
end
