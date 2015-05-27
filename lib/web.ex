defmodule Drumbeat.Parser do
  def parse_json(nil), do: nil
  def parse_json(data), do: Poison.decode!(data, as: Drumbeat.Request)
end
defmodule Drumbeat.Web do
  use Clint

  @max_timeout 100_000_000
  defp await_response(uuid, timeout \\ @max_timeout) do
    receive do
      {:http_response, response} -> {:ok, response}
    after
      timeout -> {:error, :timeout}
    end
  end

  defp first_header(conn, header) do
    case get_req_header(conn, header) do
      [x|_tail] -> x
      [] -> nil
    end
  end

  def write_headers(conn, []), do: conn
  def write_headers(conn, headers) do

    Enum.reduce headers, conn, fn
      {"Content-Length", _}, acc -> acc
      {k, v}, acc ->
        put_resp_header(acc, to_string(k), v)
    end
  end

  def write_request(conn, req) do
    write_headers(conn, req.headers)
    |> resp(:ok, case req.body do
                   x when is_map(x) -> Poison.encode!(x)
                 end)
    |> send_resp
  end

  defp build_request(conn) do
    conn
    |> first_header("x-request")
    |> Drumbeat.Parser.parse_json
    |> Drumbeat.Request.rewrite_urls(:sender_pid, self())
    |> Drumbeat.Request.add_terminal_node(Drumbeat.Request.quote_req)
    |> Drumbeat.Request.add_terminal_node(Drumbeat.Request.message_sink)
  end

  def send_response(conn, uuid) do
    case await_response(uuid) do
      {:ok, resp} -> write_request(conn, resp)
      {:error, :timeout} -> send_resp(conn, 500, Posion.encode(%{error: :timeout}))
    end
  end

  get "/json" do
    uuid = UUID.uuid4()
    request = build_request(conn)
    Drumbeat.Dispatch.place_request(Drumbeat.Dispatch, uuid, request)
    send_response(conn, uuid)
  end
end
