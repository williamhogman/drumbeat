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

  defp parse_sink(nil), do: :local
  defp parse_sink("local"), do: :local

  defp create_sink_node(conn) do
    case conn |> first_header("x-sink-to") |> parse_sink do
      :local -> Drumbeat.Request.message_sink(self())
    end
  end

  defp parse_json_request(nil), do: nil
  defp parse_json_request(data), do: Poison.decode!(data, as: Drumbeat.Request)

  defp out_json(conn, status_code, data) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status_code, data)
  end

  get "/json" do
    uuid = UUID.uuid4()
    sink = create_sink_node(conn)
    request = conn
    |> first_header("x-request")
    |> parse_json_request
    |> Drumbeat.Request.add_terminal_node(sink)
    IO.inspect(request)

    Drumbeat.Dispatch.place_request(Drumbeat.Dispatch, uuid, request)
    {:ok, {headers, body}} = await_response(uuid)

    {:ok, data} = Poison.encode(%{headers: Enum.into(headers, %{}), body: body})
    out_json(conn, 200, data)
  end
end
