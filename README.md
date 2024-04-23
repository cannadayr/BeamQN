## BeamQN

An experiment in linking [CBQN](https://github.com/dzaima/CBQN) into the Erlang BEAM as a NIF. 

Build (Development)
-----

1. Download and extract the latest Erlang/OTP release tarball from the [downloads page](https://www.erlang.org/downloads).
2. Export `$ERL_TOP` to the path of the extracted source.
3. [Build Erlang/OTP from the release tarball.](https://www.erlang.org/doc/installation_guide/install#how-to-build-and-install-erlang-otp)
4. [Build the debug enabled runtime system.](https://www.erlang.org/doc/installation_guide/install#Advanced-configuration-and-build-of-ErlangOTP_Building_How-to-Build-a-Debug-Enabled-Erlang-RunTime-System)
5. Using the compiled runtime for development:

   ##### Debug
   
       $ PATH="$ERL_TOP/bin:$PATH" rebar3 clean   
       $ PATH="$ERL_TOP/bin:$PATH" rebar3 compile
       $ PATH="$ERL_TOP/bin:$PATH" cerl -debug -pa ebin -pa ./_build/default/lib/*/ebin
       1> beamqn_test:beamqn_bqn_makeF64_tests().

   ##### ASAN

       $ export ASAN_OPTIONS="log_path=log/asan.log"
       $ export LSAN_OPTIONS="suppressions=$ERL_TOP/erts/emulator/asan/suppress"
       $ PATH="$ERL_TOP/bin:$PATH" rebar3 clean   
       $ PATH="$ERL_TOP/bin:$PATH" CFLAGS="-fsanitize=address -fno-common -fno-omit-frame-pointer" rebar3 compile
       $ PATH="$ERL_TOP/bin:$PATH" cerl -asan -pa ebin -pa ./_build/default/lib/*/ebin

   ##### Valgrind

       $ export VALGRIND_LOG_DIR=log
       $ PATH="$ERL_TOP/bin:$PATH" rebar3 clean   
       $ PATH="$ERL_TOP/bin:$PATH" rebar3 compile
       $ PATH="$ERL_TOP/bin:$PATH" cerl -valgrind -pa ebin -pa ./_build/default/lib/*/ebin
   
Build
-----

    $ rebar3 compile

Installation (Elixir)
---------------------

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `beamqn` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:beamqn, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/beamqn>.
