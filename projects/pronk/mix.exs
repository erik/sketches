defmodule Pronk.Mixfile do
  use Mix.Project

  def project do
    [app: :pronk,
     version: "0.1.0",
     elixir: "~> 1.5",
     start_permanent: Mix.env == :prod,
     deps: deps()
    ]
  end

  def application do
    [extra_applications: [:logger],
     mod: {Pronk, []}
    ]
  end

  defp deps do
    [{:poison, "~> 3.1"}]
  end
end
