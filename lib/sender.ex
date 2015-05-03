defmodule Drumbeat.Sender do
  @timeout 10000
  @connect_timeout 5000

  def start_link(dispatch, uuid, request) do
    pid = spawn_link(Drumbeat.Sender, :init, [dispatch, uuid, request])
    {:ok, pid}
  end

  defp attempt_request(request, opts) do
    %HTTPotion.Response{ headers: headers, body: body} = HTTPotion.get(request[:url], request)
    {:done, headers, body}
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
