defmodule BeamQN.MixProject do
  use Mix.Project

  def project do
    [
      app: :beamqn,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      # Prevent multiple CBQN checkouts.
      deps_path: "_build/default/lib/",
      aliases: aliases()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
      {:cbqn, git: "https://github.com/dzaima/CBQN.git", ref: "a6a0888", app: false, compile: compile_cbqn()}
    ]
  end

  defp aliases do
    [
      "deps.compile": ["deps.get", "deps.compile", fn _args -> Mix.Shell.cmd(compile_cbqn(), [cd: "#{Mix.Project.deps_path()}/cbqn"], &print_info/1) end],
      "deps.clean": ["deps.clean", &clean_cbqn/1],
      compile: ["compile", &compile_beamqn/1]
    ]
  end

  defp print_info(s) do
    Mix.Shell.IO.info(String.trim(s))
  end

  defp compile_beamqn(_args) do
    case :os.type() do
      {_family, os} when os in [:linux, :darwin, :solaris] ->
        Mix.Shell.cmd("DEPS_DIR=#{Mix.Project.deps_path()} make -C c_src", [], &print_info/1)
      {_family, os} when os in [:freebsd] ->
        Mix.Shell.cmd("DEPS_DIR=#{Mix.Project.deps_path()} gmake -C c_src", [], &print_info/1)
    end
  end

  defp compile_cbqn do
    case :os.type() do
      {_family, os} when os in [:linux, :darwin, :solaris] ->
        "make f='-DNO_MMAP' CATCH_ERRORS=1 FFI=0 static-lib"
      {_family, os} when os in [:freebsd] ->
        "gmake f='-DNO_MMAP' CATCH_ERRORS=1 FFI=0 static-lib"
    end
  end

  defp clean_cbqn(_args) do
    if File.exists?("#{Mix.Project.deps_path()}/cbqn") do
        case :os.type() do
          {_family, os} when os in [:linux, :darwin, :solaris] ->
            Mix.Shell.cmd("make clean", [cd: "#{Mix.Project.deps_path()}/cbqn"], &print_info/1)
          {_family, os} when os in [:freebsd] ->
            Mix.Shell.cmd("gmake clean", [cd: "#{Mix.Project.deps_path()}/cbqn"], &print_info/1)
        end
        case :os.type() do
          {_family, os} when os in [:linux, :darwin, :solaris, :freebsd] ->
            Mix.Shell.cmd("rm -f libcbqn.a", [cd: "#{Mix.Project.deps_path()}/cbqn"], &print_info/1)
        end
    end
  end
end
