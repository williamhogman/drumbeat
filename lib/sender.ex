alias Drumbeat.Request, as: Req
defmodule Drumbeat.Sender do
  defp perform_request(req) do
    case req.url.type do
      :http ->
        Drumbeat.Sender.HTTP.request(req)
      :message_sink ->
        send req.url.url, {:http_response, req}
        req
      :quote -> %Req{body: req, headers: []}
      :eval -> Drumbeat.Parser.parse(req.body)
    end
  end

  def run(_, id, request), do: {id, perform_request(request)}
end
