defmodule Drumbeat.Web do
  use Drumbeat.WebHelpers

  @max_timeout 100_000_000
  defp await_response(uuid, timeout \\ @max_timeout) do
    receive do
      {:http_response, response} -> {:ok, response}
    after
      timeout -> {:error, :timeout}
    end
  end

  defp write_request(conn, req) do
    write_headers(conn, req.headers)
    |> resp(:ok, case req.body do
                   x when is_map(x) -> Poison.encode!(x)
                 end)
    |> send_resp
  end

  defp send_response(conn, uuid) do
    case await_response(uuid) do
      {:ok, resp} -> write_request(conn, resp)
      {:error, :timeout} -> send_resp(conn, 500, Posion.encode(%{error: :timeout}))
    end
  end

  get "/json" do
    {new_conn, body} = read_full_body!(conn)
    uuid = UUID.uuid4()
    Drumbeat.Dispatch.place_request(Drumbeat.Dispatch, uuid, Drumbeat.Parser.parse_and_decorate(body))
    send_response(new_conn, uuid)
  end
end
