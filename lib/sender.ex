alias Drumbeat.Request, as: Req
defmodule Drumbeat.Sender do

  def start_link(dispatch, uuid, request) do
    pid = spawn_link(Drumbeat.Sender, :run, [dispatch, uuid, request])
    {:ok, pid}
  end

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

  def run(dispatch, id, request) do
    Drumbeat.Dispatch.report_response(dispatch, id, %Req{} = perform_request(request))
  end
end
