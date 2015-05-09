defmodule Drumbeat.Sender do
  @timeout 10000
  @connect_timeout 5000

  def start_link(dispatch, uuid, request) do
    pid = spawn_link(Drumbeat.Sender, :init, [dispatch, uuid, request])
    {:ok, pid}
  end

  defp attempt_request(
    %Drumbeat.Request{url: :http_uuid_response, body: body, headers: headers},
    opts
  ) do
    {:http_uuid_response, headers, body}
  end
  defp attempt_request(%Drumbeat.Request{headers: headers, url: url}, opts) do
    %HTTPotion.Response{ headers: headers, body: body} = HTTPotion.get(url, headers)
    {:done, headers, body}
  end

  defp loop(dispatch, uuid, request, opts, _state) do
    case attempt_request(request, opts) do
      {:done, headers, body} ->
        Drumbeat.Dispatch.report_response(dispatch, {uuid, headers, body})
      {:http_uuid_response, headers, body} ->
        Drumbeat.Dispatch.report_response(dispatch, {uuid, headers, body})
    end
  end

  def init(dispatch, uuid, request) do
    loop(dispatch, uuid, request, [], nil)
  end
end
