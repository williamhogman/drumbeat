defmodule Drumbeat.Web do
  use Plug.Builder
  use Clint.Util
  use Plug.ErrorHandler

  def handle_errors(conn, %{kind: _, reason: _, stack: _}) do
    send_resp(conn, conn.status, "Something went wrong!")
  end

  if Mix.env == :dev do
    use Plug.Debugger
    plug Clint.Logger
  end
  plug Drumbeat.Web.Deserializer
  plug Drumbeat.Web.Receiver
end
