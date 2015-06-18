defmodule RequestTest do
  use ExUnit.Case

  test "Successor null-case" do
    resp = %Drumbeat.Request{}
    next = %Drumbeat.Request{}
    res = Drumbeat.Request.successor(resp, next)

    assert %Drumbeat.Request{} == res
    assert res == next
    assert res == resp
  end

  test "Successor values carry" do
    resp = %Drumbeat.Request{body: :x}
    succ = Drumbeat.Request.successor(
      resp,
      %Drumbeat.Request{}
    )
    assert succ == resp
  end

  test "Successor responses don't override set values" do
    next = %Drumbeat.Request{body: :y}
    succ = Drumbeat.Request.successor(
      %Drumbeat.Request{body: :x},
      next
    )
    assert succ == next
  end

  test "Successor uses both sources" do
    next = %Drumbeat.Request{body: :y}
    succ = Drumbeat.Request.successor(
      %Drumbeat.Request{method: :z},
      next
    )
    assert succ == %Drumbeat.Request{body: :y, method: :z}
  end
end
