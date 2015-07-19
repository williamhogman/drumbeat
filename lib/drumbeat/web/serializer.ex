defmodule Drumbeat.Web.Serializer do
  import Plug.Conn
  @behaviour Plug

  def init(options), do: options

  @spec to_request(Plug.Conn.t, binary) :: Drumbeat.Request.t
  def to_request(conn, body) do
    headers = Enum.into(%{}, conn.req_headers)
    %Drumbeat.Request{body: Drumbeat.Parser.parse(body), headers: headers, method: conn.method}
    |> Drumbeat.Parser.parse
  end

  @spec read_full_body!(Plug.Conn.t) :: {Plug.Conn.t, binary}
  def read_full_body!(conn) do
    case read_full_body "", conn do
      {:ok, new_conn, body} -> {new_conn, body}
      {:error, e} -> raise e
    end
  end

  @spec read_full_body(binary, Plug.Conn.t) :: {:ok, Plug.Conn.t, binary} | {:error, term}
  def read_full_body(acc, conn) do
    case read_body(conn) do
      {:ok, body, new_conn} -> {:ok, new_conn, acc <> body}
      {:more, body, new_conn} -> read_full_body(acc <> body, new_conn)
      {:error, _} = e -> e
    end
  end

  def call(conn, _opts) do
    {new_conn, body} = read_full_body!(conn)
    req = to_request(new_conn, body)
    assign(new_conn, :parsed_body, req)
  end
end
