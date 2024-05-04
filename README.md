## BeamQN

An experiment in linking [CBQN](https://github.com/dzaima/CBQN) into the Erlang BEAM as a NIF. 

### Build (Development)

1. Download and extract the latest Erlang/OTP release tarball from the [downloads page](https://www.erlang.org/downloads).
2. Export `$ERL_TOP` to the path of the extracted source.
3. [Build Erlang/OTP from the release tarball.](https://www.erlang.org/doc/installation_guide/install#how-to-build-and-install-erlang-otp)
4. [Build the debug enabled runtime system.](https://www.erlang.org/doc/installation_guide/install#Advanced-configuration-and-build-of-ErlangOTP_Building_How-to-Build-a-Debug-Enabled-Erlang-RunTime-System)
5. Using the compiled runtime for development:

   #### Debug
   
       $ PATH="$ERL_TOP/bin:$PATH" rebar3 compile
       $ PATH="$ERL_TOP/bin:$PATH" cerl -debug +pc unicode -pa ebin -pa ./_build/default/lib/*/ebin

### Installation (Erlang)

The package can be installed by adding `beamqn` to your list of dependencies in `rebar.config`:

```erlang
{deps, [
    {rebar, {git, "git://github.com/cannadayr/BeamQN.git", {branch, "main"}}}
]}.
```

### Installation (Elixir)

The package can be installed by adding `beamqn` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:beamqn, git: "https://github.com/cannadayr/BeamQN.git", branch: "main"}
  ]
end
```

### Documentation

Documentation can be generated with [EDoc](https://www.erlang.org/doc/man/edoc).
```
rebar3 edoc
```
Once generated, the docs can be found at `/doc/`.

### Testing

Tests can be ran with [EUnit](https://www.erlang.org/doc/apps/eunit/).
```
rebar3 eunit
```
