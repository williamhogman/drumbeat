defmodule DispatchTest do
  use ExUnit.Case, async: true

  setup do
    {:ok, dispatch} = Drumbeat.Dispatch.start_link
    {:ok, dispatch: dispatch}
  end

  test "creates requests", %{dispatch: dispatch} do
    assert Drumbeat.Dispatch.get_status(dispatch, uuid) == false
    uuid = :uuid
    {:ok, uuid} = Drumbeat.Dispatch.place_request(dispatch, uuid)
    assert Drumbeat.Dispatch.get_status(dispatch, uuid) == true
  end
end
