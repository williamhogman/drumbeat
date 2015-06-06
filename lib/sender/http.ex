alias Drumbeat.Request, as: Req
defmodule Drumbeat.Sender.HTTP do
  @timeout 10000

  defp decode_if_possible data do
    case Poison.decode data do
      {:ok, decoded} -> decoded
      {:error, _x} -> data
    end
  end

  def decode_response_body resp do
    case Keyword.get(resp.headers, :"Content-Type") do
      "application/json" -> decode_if_possible(resp.body)
      _ -> resp.body
    end
  end

  defp preprocess_body(body) when is_map(body) do
    {:ok, body} = Poison.encode body
    body
  end
  defp preprocess_body(nil), do: ""
  defp preprocess_body(body), do: body

  defp preprocess_headers(nil), do: %{}
  defp preprocess_headers(headers), do: Dict.delete(headers, :"Content-Length")

  def request(r = %Req{}) do
    try do
      resp = HTTPotion.request(r.method || :get, url,
                               headers: preprocess_headers(r.headers),
                               body: preprocess_body(r.body),
                               timeout: @timeout)
      %Req{headers: Enum.into(resp.headers, %{}),
           body: decode_response_body(resp)}
    rescue
      e in HTTPotion.HTTPError ->
        IO.inspect([method, url, headers, body])
        IO.inspect(e)
        e
    end

  end
end
