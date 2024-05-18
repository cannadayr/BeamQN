## BeamQN

An experiment in linking [CBQN](https://github.com/dzaima/CBQN) into the Erlang BEAM as a NIF.

### Status

BeamQN is experimental software and is **not ready for production use**. Use at your own risk!

The risks of using this library include, but are not limited to:
* Memory leaks
* Scheduler collapse
* Undefined behavior
* System crash

In order to rapidly identify bugs, it is recommended to develop BeamQN applications using a debug enabled Erlang runtime system.
A general guide for building a debug enabled Erlang runtime system and the latest Rebar3 build tool is below.

### Build (Development)

1. Download and extract the latest Erlang/OTP release tarball from the [downloads page](https://www.erlang.org/downloads).
2. Export `$ERL_TOP` to the path of the extracted source.
3. [Build Erlang/OTP from the release tarball.](https://www.erlang.org/doc/installation_guide/install#how-to-build-and-install-erlang-otp)
4. [Build the debug enabled runtime system.](https://www.erlang.org/doc/installation_guide/install#Advanced-configuration-and-build-of-ErlangOTP_Building_How-to-Build-a-Debug-Enabled-Erlang-RunTime-System)
6. [Install Rebar3 from source using the latest Erlang/OTP release tarball.](https://rebar3.org/docs/getting-started/#installing-from-source)
```
env PATH=$ERL_TOP:$PATH ./bootstrap
```
7. Export `$REBAR_TOP` to the path of the rebar3 source.
8. Using the compiled runtime for development:

   #### Debug
       $ env PATH=$ERL_TOP/bin:$PATH $REBAR_TOP/rebar3 compile
       $ env PATH=$ERL_TOP/bin:$PATH cerl -debug +pc unicode -pa ebin -pa ./_build/default/lib/*/ebin

### Installation (Erlang)

The package can be installed by adding `beamqn` to your list of dependencies in `rebar.config`:

```erlang
{deps, [
    {beamqn, {git, "https://github.com/cannadayr/BeamQN.git", {branch, "main"}}}
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
env PATH=$ERL_TOP/bin:$PATH $REBAR_TOP/rebar3 edoc
```
Once generated, the docs can be found at `/doc/`.

### Testing

Tests can be ran with [EUnit](https://www.erlang.org/doc/apps/eunit/).
```
env PATH=$ERL_TOP/bin:$PATH $REBAR_TOP/rebar3 eunit
```
