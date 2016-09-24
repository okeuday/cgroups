defmodule CGroups.Mixfile do
  use Mix.Project

  def project do
    [app: :cgroups,
     version: "1.5.3",
     language: :erlang,
     description: description,
     package: package,
     deps: deps]
  end

  def application do
    [env: [
       version_default: 2,
       version_default_required: false,
       path_v1: '/sys/fs/cgroup/',
       path_v2: '/sys/fs/cgroup2/',
       path_mounts: '/proc/mounts']]
  end

  defp deps do
    []
  end

  defp description do
    "Erlang native cgroups interface"
  end

  defp package do
    [files: ~w(src doc rebar.config README.markdown LICENSE),
     maintainers: ["Michael Truog"],
     licenses: ["BSD"],
     links: %{"GitHub" => "https://github.com/okeuday/cgroups"}]
   end
end
