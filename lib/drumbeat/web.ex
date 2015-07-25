defmodule Drumbeat.Web do
  use Drumbeat.Web.Helpers
  plug Drumbeat.Web.Deserializer
  plug Drumbeat.Web.Receiver
end
