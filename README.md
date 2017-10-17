crier
=====

A partial implementation of the IRC protocol.

This is not meant to be used in production, but a project for me to learn more
about concurrency and the IRC protocol.

Usage
-----

    $ rebar3 release
    $ ./_build/default/rel/crier/bin/crier <start|foreground>
    
Notes
-----

This will run the IRC server on `localhost/5000` (IRC format, `localhost:5000`).

Implementation status can partially be found in the `TODO.org` file, a more
detailed list will be added to this file later.
