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
    request = %Drumbeat.Request{url: 'http://httpbin.org/status/200',
                                headers: [],
                                respond_to: Drumbeat.Request.message_sink(self() ),
                               }
    Drumbeat.Dispatch
    |> Drumbeat.Dispatch.place_request(uuid, request)

    IO.inspect(await_response(uuid))
    conn
    |> text('ok')
  end
end
