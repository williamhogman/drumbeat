defmodule Drumbeat.Sender do
  @timeout 10000
  @connect_timeout 5000

  def start_link(dispatch, url) do
    spawn_link(Drumbeat.Sender, :init, [dispatch, url])
  end

  defp attempt_request(_url, _opts) do
    {:done, [{:foo, "bar"}], "data"}
  end

  defp loop(dispatch, url, opts, _state) do
    case attempt_request(url, opts) do
      {:done, headers, body} ->
        Drumbeat.Dispatch.report_response(dispatch, {headers, body})
    end
  end

  def init(dispatch, url) do
    loop(dispatch, url, [], nil)
  end
end
