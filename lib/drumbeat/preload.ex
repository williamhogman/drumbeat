defmodule Drumbeat.Preload do
  alias Drumbeat.Parser
  alias Drumbeat.Request
  def build_request do
    ["json", "sync","raw" ]
    |> Enum.map(&build/1)
  end

  defp build(url) do
    build_part(url, template_for(url))
  end

  defp build_part(url, fname) do
    data = Parser.from_file(fname)
    body = %Request{body: data, url: url}
    %Request{type: :http_hook, body: body}
  end

  defp template_for("json"), do: "templates/json.json"
  defp template_for("sync"), do: "templates/sync.json"
  defp template_for("raw"), do: "templates/raw.json"
  defp template_for(_), do: "templates/raw.json"
end
