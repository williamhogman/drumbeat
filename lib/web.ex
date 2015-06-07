defmodule Drumbeat.Web do
  use Drumbeat.WebHelpers

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

  def to_request(conn, body) do
    headers = Enum.into(%{}, conn.req_headers)
    %Drumbeat.Request{body: Drumbeat.Parser.parse(body), headers: headers, method: conn.method}
    |> Drumbeat.Parser.parse
  end

  defp perform_request(reqs) do
    Drumbeat.Dispatch.place_request(Drumbeat.Dispatch, UUID.uuid4(), reqs)
    await_response()
  end

  defp template_for("json"), do: "templates/json.json"
  defp template_for("sync"), do: "templates/sync.json"
  defp template_for("raw"), do: "templates/sync.json"
  defp template_for(_), do: "templates/raw.json"

  match "/*path" do
    {new_conn, body} = read_full_body!(conn)
    body_reqs = %Drumbeat.Request{type: :eval, body: to_request(new_conn, body)}
    reqs = path |> template_for |> Drumbeat.Parser.with_template(body_reqs)
    send_response(new_conn, perform_request(reqs))
  end
end
