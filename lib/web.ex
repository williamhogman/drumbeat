defmodule Drumbeat.Parser do
  def parse_json(nil), do: nil
  def parse_json(data), do: Poison.decode!(data, as: Drumbeat.Request)

  def parse_sink(nil), do: :local
  def parse_sink("local"), do: :local
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

  get "/status" do
    conn
    |> text('drumbeat')
  end

  get "/favicon.ico" do
    conn
    |> text('no')
  end

  defp first_header(conn, header) do
    case get_req_header(conn, header) do
      [x|_tail] -> x
      [] -> nil
    end
  end

  defp create_sink_node(conn) do
    case conn |> first_header("x-sink-to") |> Drumbeat.Parser.parse_sink do
      :local -> Drumbeat.Request.message_sink(self())
    end
  end

  def write_request(conn, req) do
    Enum.reduce req.headers, conn, fn {k, v}, acc -> put_resp_header(acc, k, v) end
    |> resp(:ok, case req.body do
                        x when is_map(x) ->
                          {:ok, data} = Poison.encode(%{headers: Enum.into(x.headers, %{}), body: x.body})
                          data
                      end)
    |> send_resp
  end

  defp build_request(sink, conn) do
    conn
    |> first_header("x-request")
    |> Drumbeat.Parser.parse_json
    |> Drumbeat.Request.rewrite_urls(:sender_pid, self())
    |> Drumbeat.Request.add_terminal_node(sink)
  end

  def send_response(conn, uuid) do
    case await_response(uuid) do
      {:ok, resp} ->
        # {:ok, data} = Poison.encode(%{headers: Enum.into(resp.headers, %{}), body: resp.body})
        #out_json(conn, 200, data)
        write_request(conn, %Drumbeat.Request{body: resp, headers: [] })
      {:error, :timeout} ->
        out_json(conn, 500, %{error: :timeout})
    end
  end

  get "/json" do
    uuid = UUID.uuid4()
    sink = create_sink_node(conn)
    request = build_request(sink, conn)

    Drumbeat.Dispatch.place_request(Drumbeat.Dispatch, uuid, request)
    send_response(conn, uuid)
  end
end
