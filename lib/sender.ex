defmodule Drumbeat.Sender.HTTP do
  @timeout 10000

  defp decode_if_possible data do
    case Poison.decode data do
      {:ok, decoded} -> decoded
      {:error, _x} -> data
    end
  end

  def decode_response_body resp do
    case Keyword.get(resp.headers, :"Content-Type") do
      "application/json" -> decode_if_possible(resp.body)
      _ -> resp.body
    end
  end

  defp preproces_body(body) when is_map(body) do
    case Poison.encode body do
      {:ok, body} -> body
    end
  end
  defp preproces_body(body), do: body

  def request(method, url, headers, body) do
    resp = HTTPotion.request(method || :get, url,
                             headers: headers || [],
                             body: preproces_body(body),
                             timeout: @timeout
    )
    %Drumbeat.Request{headers: resp.headers,
                      body: decode_response_body(resp)}
  end
end

defmodule Drumbeat.Sender do

  def start_link(dispatch, uuid, request) do
    pid = spawn_link(Drumbeat.Sender, :init, [dispatch, uuid, request])
    {:ok, pid}
  end

  defp attempt_request(req) do

    case req.url.type do
      :http ->
        Drumbeat.Sender.HTTP.request(req.method, req.url.url, req.headers, req.body)
      :message_sink ->
        send req.url.url, {:http_response, req}
        req
      :quote ->
        %Drumbeat.Request{body: req, headers: []}
    end
  end

  defp loop(dispatch, uuid, request) do
    case attempt_request(request) do
      %Drumbeat.Request{headers: headers, body: body} ->
        Drumbeat.Dispatch.report_response(dispatch, {uuid, headers, body})
    end
  end

  def init(dispatch, uuid, request) do
    loop(dispatch, uuid, request)
  end
end
