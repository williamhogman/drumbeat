defmodule Drumbeat.Sender.HTTP do

  defp decode_if_possible data do
    case Poison.decode data do
      {:ok, decoded} -> decoded
      {:error, _x} -> data
    end
  end

  def request(method, url, headers) do
    resp = HTTPotion.request(method || :get, url, headers || [])
    decoded_body = case Keyword.get(resp.headers, :"Content-Type") do
                     "application/json" -> decode_if_possible(resp.body)
                     _ -> resp.body
                   end
    {resp.headers, decoded_body}
  end
end

defmodule Drumbeat.Sender do
  @timeout 10000
  @connect_timeout 5000

  def start_link(dispatch, uuid, request) do
    pid = spawn_link(Drumbeat.Sender, :init, [dispatch, uuid, request])
    {:ok, pid}
  end

  defp attempt_request(
    %Drumbeat.Request{url: {:message_sink, pid}, body: body, headers: headers}, opts
  ) do
    send pid, {:http_response, {headers, body}}
    {:done, headers, body}
  end
  defp attempt_request(%Drumbeat.Request{headers: headers, url: url, method: method}, opts) when is_list(url) or is_binary(url) do
    {response_headers, response_body} = Drumbeat.Sender.HTTP.request(method, url, headers)
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
