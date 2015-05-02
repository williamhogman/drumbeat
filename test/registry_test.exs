defmodule RegistryTest do
  use ExUnit.Case, async: true

  setup do
    {:ok, registry} = Drumbeat.Registry.start_link
    {:ok, registry: registry}
  end

  test "registers incoming request", %
  {registry: registry} do
    uuid = "abcdef-abcdef-abcef-deef"
    assert Drumbeat.Registry.has_request(registry, uuid) == false
    :ok = Drumbeat.Registry.place_request(
      registry, uuid, url: 'http://example.com/'
    )
  end
end
