defmodule WebDerserializerTest do
  alias Drumbeat.Request
  alias Drumbeat.Web.Deserializer
  use ExUnit.Case, async: true
  use Plug.Test

  defp json_conn(data) do
    conn(:post, "arbitrary", data)
    |> put_req_header("content-type", "application/json")
  end

  @opts Deserializer.init([])
  defp call_test(fixture), do: Deserializer.call(fixture, @opts)

  defp call_body(body), do: body |> json_conn |> call_test

  defp sanity_check(parsed_body) do
    assert parsed_body.method == :post
    assert :proplists.get_value("content-type", parsed_body.headers) == "application/json"
  end

  defp parsed_body(res) do
    assert Map.has_key?(res.assigns, :parsed_body)
    res.assigns[:parsed_body]
  end

  test "deserializes empty lists" do
    res = call_body("[]")
    parsed_body = parsed_body(res)
    sanity_check parsed_body
    assert parsed_body.body == []
  end

  test "deserializes lists of requests" do
    res = call_body """
    [
      {
        "url": "foo"
      }
    ]
    """
    parsed_body = parsed_body(res)
    sanity_check(parsed_body)
    assert List.first(parsed_body.body).url == "foo"
  end

end
