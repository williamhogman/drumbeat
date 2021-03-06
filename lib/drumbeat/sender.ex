alias Drumbeat.Request, as: Req
defmodule Drumbeat.Sender do
  @spec perform_request(Req.t) :: Req.t
  defp perform_request(req) do
    case req.type do
      :http ->
        Drumbeat.Sender.HTTP.request(req)
      :message_sink ->
        send req.url, {:http_response, req}
        req
      :quote -> %Req{body: req, headers: []}
      :eval ->
        Drumbeat.Parser.parse(req.body.body)
      :http_hook ->
        :ok = Drumbeat.HooksStore.put({:http_hook, :ns, req.body.url}, Drumbeat.Parser.parse(req.body.body))
        %Req{body: {saved: true}}
    end
  end

  @spec run(binary, Req.t) :: {binary, Req.t}
  def run(id, request), do: {id, perform_request(request)}
end
