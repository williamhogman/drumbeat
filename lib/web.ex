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
  defp out_json(conn, status_code, data) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status_code, data)
  end

  defp build_request(sink, conn) do
    conn
    |> first_header("x-request")
    |> Drumbeat.Parser.parse_json
    |> Drumbeat.Request.rewrite_urls(:sender_pid, self())
    |> Drumbeat.Request.add_terminal_node(sink)
  end

  get "/json" do
    uuid = UUID.uuid4()
    sink = create_sink_node(conn)
    request = build_request(sink, conn)

    Drumbeat.Dispatch.place_request(Drumbeat.Dispatch, uuid, request)
    {:ok, {headers, body}} = await_response(uuid)

    {:ok, data} = Poison.encode(%{headers: Enum.into(headers, %{}), body: body})
    out_json(conn, 200, data)
  end
end
