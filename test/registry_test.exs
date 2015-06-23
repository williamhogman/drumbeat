defmodule RegistryTest do
  alias Drumbeat.Registry
  use ExUnit.Case

  defp place_and_advance(key, initial, resp \\ []) do
    Registry.new
    |> Registry.place_request(key, initial)
    |> Registry.advance_request(:a, resp)
  end

  test "Identity for null-element" do
    assert Registry.new == Registry.new, "empty regs are equal"
  end

  test "Placing requests is observable" do
    {placed, reg} = place_and_advance(:a, [:current, :next, :after_next])
    assert placed == [:next, :after_next], "Remaining requests are obtained"
    assert reg == Registry.new, "The resulting registry is empty"
  end

  test "Advancing non-existant requests throws" do
    reg = Registry.new
    assert catch_error(Registry.advance_request(:a, []))
  end

  test "Eval responses are appended" do
    {placed, reg} = place_and_advance(:a, [:skip, :y, :z], [:w, :x])
    assert placed == [:w, :x, :y, :z], "Multi-response before body"
  end

  test "Final request adds a single request" do
    {placed, reg} = place_and_advance(:a, [:x], [:y])
    assert placed == [:y]
  end

  test "Final request returns nil" do
    {placed, reg} = place_and_advance(:a, [:x], [])
    assert placed == nil
  end


end
