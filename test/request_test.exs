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

  test "Successors all keys carry" do
    resp = %Drumbeat.Request{
                        body: :a,
                        headers: :b,
                        method: :c,
                        url: :d,
                        type: :e
                    }
    next = %Drumbeat.Request{}
    succ = Drumbeat.Request.successor(
      %Drumbeat.Request{},
      resp
    )
    assert resp == succ
  end

  test "Rewriting url when match" do
    prior = %Drumbeat.Request{url: :a}
    [res|[]] = Drumbeat.Request.rewrite_urls([prior], :a, :b)
    assert res.url == :b
  end

  test "Not rewriting urls when mismatched " do
    prior = %Drumbeat.Request{url: :c}
    [res|[]] = Drumbeat.Request.rewrite_urls([prior], :a, :b)
    assert res.url == :c
  end

  test "Rewriting multiple urls" do
    a = %Drumbeat.Request{url: :a}
    [a1_conv, a2_conv] = Drumbeat.Request.rewrite_urls([a, a], :a, :c)
    assert a1_conv.url == :c
    assert a2_conv.url == :c
  end

  test "Not rewriting some urls" do
    a = %Drumbeat.Request{url: :a}
    b = %Drumbeat.Request{url: :b}
    [a_conv, b_conv] = Drumbeat.Request.rewrite_urls([a, b], :a, :c)
    assert a_conv.url == :c
    assert b_conv.url == :b
  end

  test "message_sink template works" do
    res = Drumbeat.Request.message_sink(:a)
    assert res.type == :message_sink
    assert res.url == :a
  end

  test "implied message sink works" do
    res = Drumbeat.Request.message_sink
    assert res.type == :message_sink
    assert res.url == self()
  end

  test "quote_req works" do
    res = Drumbeat.Request.quote_req
    assert res.type == :quote
  end

end
