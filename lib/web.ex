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
    write_headers(conn, req.headers)
    |> resp(:ok, case req.body do
                   x when is_map(x) -> Poison.encode!(x)
                 end)
    |> send_resp
  end

  defp send_response(conn, {:ok, resp}) do
    write_request(conn, resp)
  end
  defp send_response(conn, {:error, :timeout}) do
    send_resp(conn, 500, Posion.encode(%{error: :timeout}))
  end

  defp perform_request(body) do
    Drumbeat.Dispatch.place_request(Drumbeat.Dispatch, UUID.uuid4(), Drumbeat.Parser.parse_and_decorate(body))
    await_response()
  end

  match "/*_path" do
    {new_conn, body} = read_full_body!(conn)
    send_response(conn, perform_request(body))
  end
end
