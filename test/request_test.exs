defmodule RequestTest do
  alias Drumbeat.Request, as: Req
  use ExUnit.Case

  test "Successor null-case" do
    resp = %Req{}
    next = %Req{}
    res = Req.successor(resp, next)

    assert %Req{} == res
    assert res == next
    assert res == resp
  end

  test "Successor values carry" do
    resp = %Req{body: :x}
    succ = Req.successor(
      resp,
      %Req{}
    )
    assert succ == resp
  end

  test "Successor responses don't override set values" do
    next = %Req{body: :y}
    succ = Req.successor(
      %Req{body: :x},
      next
    )
    assert succ == next
  end

  test "Successor uses both sources" do
    next = %Req{body: :y}
    succ = Req.successor(
      %Req{method: :z},
      next
    )
    assert succ == %Req{body: :y, method: :z}
  end

  test "Successors all keys carry" do
    resp = %Req{
                        body: :a,
                        headers: :b,
                        method: :c,
                        url: :d,
                        type: :e
                    }
    succ = Req.successor(
      %Req{},
      resp
    )
    assert resp == succ
  end

  test "Rewriting url when match" do
    prior = %Req{url: :a}
    [res|[]] = Req.rewrite_urls([prior], :a, :b)
    assert res.url == :b
  end

  test "Not rewriting urls when mismatched " do
    prior = %Req{url: :c}
    [res|[]] = Req.rewrite_urls([prior], :a, :b)
    assert res.url == :c
  end

  test "Rewriting multiple urls" do
    a = %Req{url: :a}
    [a1_conv, a2_conv] = Req.rewrite_urls([a, a], :a, :c)
    assert a1_conv.url == :c
    assert a2_conv.url == :c
  end

  test "Not rewriting some urls" do
    a = %Req{url: :a}
    b = %Req{url: :b}
    [a_conv, b_conv] = Req.rewrite_urls([a, b], :a, :c)
    assert a_conv.url == :c
    assert b_conv.url == :b
  end

  test "message_sink template works" do
    res = Req.message_sink(:a)
    assert res.type == :message_sink
    assert res.url == :a
  end

  test "implied message sink works" do
    res = Req.message_sink
    assert res.type == :message_sink
    assert res.url == self()
  end

  test "quote_req works" do
    res = Req.quote_req
    assert res.type == :quote
  end
end

defmodule RequestDecoderTest do
  alias Drumbeat.Request, as: Req
  use ExUnit.Case

  defp decode(data) do
    Poison.Decoder.decode data,  as: Req
  end

  defp all_match(results, expected) do
    Enum.zip(results, expected)
    |> Enum.each fn ({res, exp}) ->
      assert res == exp
    end
  end

  test "HTTP is default" do
    res = decode %Req{}
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
      decode(%Req{type: x}).type
    end)
    |> all_match(Enum.map(cases, &String.to_atom/1))
  end

  test "Invalid types throw" do
    base = %Req{type: "foo"}
    assert catch_error(decode base)
  end

  test "All valid http methods works" do
    cases = [
      "head",
      "get",
      "post",
      "put",
      "patch",
      "delete",
    ]
    cases |> Enum.map(fn (x) ->
      decode(%Req{method: x}).method
    end)
    |> all_match(Enum.map(cases, &String.to_atom/1))
  end

  test "Uppercase methods works" do
    cases = [
      "head",
      "get",
      "post",
      "put",
      "patch",
      "delete",
    ]
    cases |> Enum.map(fn (x) ->
      decode(%Req{method: String.upcase(x)}).method
    end)
    |> all_match(Enum.map(cases, &String.to_atom/1))
  end

end
