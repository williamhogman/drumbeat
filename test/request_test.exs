defmodule RequestTest do
  alias Drumbeat.Request, as: Req
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

defmodule RequestDecoderTest do
  alias Drumbeat.Request, as: Req
  use ExUnit.Case

  defp decode(data) do
    Poison.Decoder.decode data,  as: Drumbeat.Request
  end

  defp all_match(results, expected) do
    IO.inspect(results)
    IO.inspect(expected)
    Enum.zip(results, expected)
    |> Enum.each fn ({res, exp}) ->
      assert res == exp
    end
  end

  test "HTTP is default" do
    res = decode %Drumbeat.Request{}
    assert res.type == :http
  end

  test "Actual types become symbols" do
    cases = [
      "http",
      "message_sink",
      "quote",
      "placeholder",
      "eval",
    ]
    cases |> Enum.map(fn (x) ->
      decode(%Drumbeat.Request{type: x}).type
    end)
    |> all_match(Enum.map(cases, &String.to_atom/1))
  end

  test "Invalid types throw" do
    base = %Drumbeat.Request{type: "foo"}
    assert catch_error(decode base)
  end

  test "All valid http methods works" do
    cases = [
      "head",
      "get",
      "post",
      "patch",
      "delete",
    ]
    cases |> Enum.map(fn (x) ->
      decode(%Drumbeat.Request{method: x}).method
    end)
    |> all_match(Enum.map(cases, &String.to_atom/1))
  end
end
