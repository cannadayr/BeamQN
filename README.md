## BeamQN

An experiment in linking [CBQN](https://github.com/dzaima/CBQN) into the Erlang BEAM as a NIF. 

Build (Development)
-----

1. Download and extract the latest Erlang/OTP release tarball from the [downloads page](https://www.erlang.org/downloads).
2. Set `$ERL_TOP` to the path of the extracted source.
3. [Build Erlang/OTP from the release tarball.](https://www.erlang.org/doc/installation_guide/install#how-to-build-and-install-erlang-otp)
4. [Build the debug enabled runtime system.](https://www.erlang.org/doc/installation_guide/install#Advanced-configuration-and-build-of-ErlangOTP_Building_How-to-Build-a-Debug-Enabled-Erlang-RunTime-System)
5. Using the compiled runtime for development:

       $ PATH="$ERL_TOP/bin:$PATH" rebar3 clean   
       $ PATH="$ERL_TOP/bin:$PATH" rebar3 compile
       $ PATH="$ERL_TOP/bin:$PATH" cerl -debug -pa ebin -pa ./_build/default/lib/*/ebin

Build
-----

    $ rebar3 compile
