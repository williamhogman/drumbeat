defmodule Drumbeat.Web do
  use Clint

  @max_timeout 100_000_000
  defp await_response(uuid, timeout \\ @max_timeout) do
    receive do
      {:http_response, response} -> {:ok, response}
    after
      timeout -> {:error, :timeout}
    end
  end

  get "/status" do
    conn
    |> text('drumbeat')
  end

  get "/pow" do
    uuid = UUID.uuid4()
    request = %Drumbeat.Request{url: 'http://httpbin.org',
                                headers: [],
                                respond_to: Drumbeat.Request.message_sink(self() ),
                               }
    Drumbeat.Dispatch
    |> Drumbeat.Dispatch.place_request(uuid, request)

    conn
    |> text(await_response(uuid))
  end
end
