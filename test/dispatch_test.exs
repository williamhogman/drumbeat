defmodule DispatchTest do
  use ExUnit.Case, async: true

  setup do
    {:ok, dispatch} = Drumbeat.Dispatch.start_link(self())
    {:ok, dispatch: dispatch}
  end

  test "creates requests", %{dispatch: dispatch} do
    uuid = '123'
    assert Drumbeat.Dispatch.get_status(dispatch, uuid) == false
    {:ok, uuid} = Drumbeat.Dispatch.place_request(dispatch, uuid, %Drumbeat.Request{url: 'http://httpbin.org/', headers: %{}})
    receive do
      x -> IO.inspect(x)
    end
    assert Drumbeat.Dispatch.get_status(dispatch, uuid) == true
  end
end
