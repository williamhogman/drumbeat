defmodule SenderTest do
  use ExUnit.Case, async: true

  test "Performs requests" do
    pid = self()
    request = %Drumbeat.Request{url: 'http://httpbin.org/status/200', headers: %{}}

    spawn(fn -> Drumbeat.Sender.init(pid, 1, request) end)
    assert_receive {_,
                     {:report_response, {1, _headers, _body}}}, 5000

  end
end
