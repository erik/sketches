defmodule Layabout.Mixfile do
  use Mix.Project

  def project do
    [app: :layabout,
     version: "0.1.0",
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    # Specify extra applications you'll use from Erlang/Elixir
    [extra_applications: [:logger, :slack],
     mod: {Layabout, []}]
  end

  defp deps do
    [{:slack, "~> 0.9"},
     {:timex, "~> 3.1.7"}]
  end
end
