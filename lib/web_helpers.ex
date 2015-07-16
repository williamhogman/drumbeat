defmodule Drumbeat.WebHelpers do
  # Various helpers making it easier to with Plug and Clint
  defmacro __using__(_) do
    quote do
      import Plug.Conn
      use Plug.Builder
      use Clint.Util
      use Plug.ErrorHandler

      if Mix.env == :dev do
        use Plug.Debugger
      end

      if Mix.env == :dev do
        plug Clint.Logger
      end

      def handle_errors(conn, %{kind: _, reason: _, stack: _}) do
        send_resp(conn, conn.status, "Something went wrong!")
      end

      @spec write_headers(Plug.Conn.t, [{binary, binary}]) :: Plug.Conn.t
      def write_headers(conn, []), do: conn
      def write_headers(conn, headers) do
        Enum.reduce headers, conn, fn
          {"Content-Length", _}, acc -> acc
          {k, v}, acc ->
            put_resp_header(acc, to_string(k), v)
        end
      end

    end
  end
end
