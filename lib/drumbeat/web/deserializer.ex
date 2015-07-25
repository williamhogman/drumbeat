defmodule Drumbeat.Web.Deserializer do
  alias Drumbeat.Request
  import Plug.Conn
  @behaviour Plug

  def init(options), do: options

  @spec to_request(Plug.Conn.t, binary) :: {:ok, Request.t} | {:error, :syntax_error}
  defp to_request(conn, body) do
    headers = Enum.into(%{}, conn.req_headers)
    try do
      parsed_body = Drumbeat.Parser.parse_json(body)
      req = %Request{body: parsed_body, headers: headers, method: conn.method}
      |> Drumbeat.Parser.parse
      {:ok, req}
    rescue
      e in Poison.SyntaxError -> {:error, :syntax_error}
    end
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

  @syntax_error_json "{ \"error\": \"JSON syntax error\"}"
  def call(conn, _opts) do
    {new_conn, body} = read_full_body!(conn)
    case to_request(new_conn, body) do
      {:ok, req} -> assign(new_conn, :parsed_body, req)
      {:error, :syntax_error} ->
        send_resp(conn, 400, @syntax_error_json)
    end

  end
end
