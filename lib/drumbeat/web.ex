defmodule Drumbeat.Web do
  use Plug.Builder
  use Plug.ErrorHandler

  def handle_errors(conn, %{kind: _, reason: _, stack: _}) do
    send_resp(conn, conn.status, "Something went wrong!")
  end

  def start do
    Plug.Adapters.Cowboy.http(__MODULE__, [], port: port)
  end

  defp port do
    System.get_env("PORT") || 4000
  end

  if Mix.env == :dev do
    use Plug.Debugger
  end
  plug Drumbeat.Web.Logger
  plug Drumbeat.Web.Deserializer
  plug Drumbeat.Web.Receiver
end
