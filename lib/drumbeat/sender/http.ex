alias Drumbeat.Request, as: Req
defmodule Drumbeat.Sender.HTTP do
  @timeout 10000

  @spec decode_if_possible(binary) :: binary | Map
  defp decode_if_possible data do
    case Poison.decode data do
      {:ok, decoded} -> decoded
      {:error, _x} -> data
    end
  end

  @spec decode_response_body(HTTPotion.Response.t) :: binary | Map
  def decode_response_body resp do
    case Keyword.get(resp.headers, :"Content-Type") do
      "application/json" -> decode_if_possible(resp.body)
      _ -> resp.body
    end
  end

  @spec preprocess_body(map | binary | nil) :: binary
  defp preprocess_body(body) when is_map(body) do
    {:ok, body} = Poison.encode body
    body
  end
  defp preprocess_body(nil), do: ""
  defp preprocess_body(body) when is_binary(body), do: body

  @spec preprocess_headers(nil | map) :: map
  defp preprocess_headers(nil), do: %{}
  defp preprocess_headers(headers), do: Dict.delete(headers, :"Content-Length")

  @spec request(Req.t) :: Req.t
  def request(r = %Req{}) do
    resp = HTTPotion.request(r.method || :get, r.url,
                             headers: preprocess_headers(r.headers),
                             body: preprocess_body(r.body),
                             timeout: @timeout)
    %Req{headers: Enum.into(resp.headers, %{}),
         body: decode_response_body(resp)}
  end
end
