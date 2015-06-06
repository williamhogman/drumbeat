alias Drumbeat.Request, as: Req
defmodule Drumbeat.Sender do

  def start_link(dispatch, uuid, request) do
    pid = spawn_link(Drumbeat.Sender, :init, [dispatch, uuid, request])
    {:ok, pid}
  end

  defp attempt_request(req) do
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

  defp loop(dispatch, uuid, request) do
    case attempt_request(request) do
      %Req{} = req ->
        Drumbeat.Dispatch.report_response(dispatch, {uuid, req})
    end
  end

  def init(dispatch, uuid, request) do
    loop(dispatch, uuid, request)
  end
end
