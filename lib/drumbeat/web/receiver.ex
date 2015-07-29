defmodule Drumbeat.Web.Receiver do
  import Plug.Conn
  alias Drumbeat.HooksStore

  @spec write_headers(Plug.Conn.t, [{binary, binary}]) :: Plug.Conn.t
  defp write_headers(conn, []), do: conn
  defp write_headers(conn, headers) do
    Enum.reduce headers, conn, fn
      {"Content-Length", _}, acc -> acc
      {k, v}, acc ->
        put_resp_header(acc, to_string(k), v)
    end
  end

  @max_timeout 100_000_000
  defp await_response(timeout \\ @max_timeout) do
    receive do
      {:http_response, response} -> {:ok, response}
    after
      timeout -> {:error, :timeout}
    end
  end

  defp write_request(conn, req) do
    write_headers(conn, req.headers || [])
    |> resp(:ok, case req.body do
                   x when is_map(x) or is_list(x)-> Poison.encode!(x)
                 end)
    |> send_resp
  end

  defp send_response(conn, {:ok, resp}) do
    write_request(conn, resp)
  end

  defp send_response(conn, {:error, :timeout}) do
    send_resp(conn, 500, Posion.encode(%{error: :timeout}))
  end

  defp perform_request(reqs) do
    Drumbeat.Dispatch.place_request(Drumbeat.Dispatch, UUID.uuid4(), reqs)
    await_response()
  end

  defp template_for("json"), do: "templates/json.json"
  defp template_for("sync"), do: "templates/sync.json"
  defp template_for("raw"), do: "templates/raw.json"
  defp template_for(_), do: "templates/raw.json"

  def init(options), do: options

  def call(conn, opts) do
    path = Enum.map(conn.path_info, &URI.decode/1)
    body_reqs = %Drumbeat.Request{type: :eval, body: conn.assigns[:parsed_body]}
    reqs = path |> List.first |> template_for |> Drumbeat.Parser.with_template(body_reqs)
    send_response(conn, perform_request(reqs))
  end
end
