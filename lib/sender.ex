defmodule Drumbeat.Sender.HTTP do
  @timeout 10000

  defp decode_if_possible data do
    case Poison.decode data do
      {:ok, decoded} -> decoded
      {:error, _x} -> data
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

    decoded_body = case Keyword.get(resp.headers, :"Content-Type") do
                     "application/json" -> decode_if_possible(resp.body)
                     _ -> resp.body
                   end
    {resp.headers, decoded_body}
  end
end

defmodule Drumbeat.Sender do

  def start_link(dispatch, uuid, request) do
    pid = spawn_link(Drumbeat.Sender, :init, [dispatch, uuid, request])
    {:ok, pid}
  end

  defp attempt_request(
    %Drumbeat.Request{url: %Drumbeat.URL{type: :message_sink, url: pid}} = req, opts
  ) do
    send pid, {:http_response, req}
    {:done, req.headers, req.body}
  end
  defp attempt_request(
    %Drumbeat.Request{url: %Drumbeat.URL{type: http, url: url}} = req, opts
  ) do
    {response_headers, response_body} =
      Drumbeat.Sender.HTTP.request(req.method, url, req.headers, req.body)
    {:done, response_headers, response_body}
  end
  defp loop(dispatch, uuid, request, opts, _state) do
    case attempt_request(request, opts) do
      {:done, headers, body} ->
        Drumbeat.Dispatch.report_response(dispatch, {uuid, headers, body})
    end
  end

  def init(dispatch, uuid, request) do
    loop(dispatch, uuid, request, [], nil)
  end
end
