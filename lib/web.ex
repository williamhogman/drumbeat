defmodule Drumbeat.Web do
  use Clint

  get "/status" do
    conn
    |> text('drumbeat')
  end

  get "/pow" do
    uuid = UUID.uuid4()
    request = %Drumbeat.Request{url: 'http://httpbin.org',
                                headers: []}
    Drumbeat.Dispatch
    |> Drumbeat.Dispatch.place_request(uuid, request)

    conn
    |> text(uuid)
  end
end
