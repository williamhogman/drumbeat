defmodule Drumbeat.WebHelpers do
  # Various helpers making it easier to with Plug and Clint
  defmacro __using__(_) do
    quote do
      use Clint
      @spec write_headers(Plug.Conn.t, [{binary, binary}]) :: Plug.Conn.t
      def write_headers(conn, []), do: conn
      def write_headers(conn, headers) do
        Enum.reduce headers, conn, fn
          {"Content-Length", _}, acc -> acc
          {k, v}, acc ->
            put_resp_header(acc, to_string(k), v)
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
          {:more, body, new_conn} -> read_full_body(acc <> body, conn)
          {:error, _} = e -> e
        end
      end

    end
  end
end
