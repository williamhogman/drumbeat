defmodule Drumbeat.Web do
  use Clint

  get "/status" do
    conn
    |> text('drumbeat')
  end
end
