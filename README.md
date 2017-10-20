crier
=====

**IMPORTANT NOTE:**

This was not supposed to evolve into the next official IRC protocol implementation
or anything like that. This was meant to be a _fun_ way for me to learn erlang, and
truth be told, it was quite fun.

Though, as it was my first interaction with IRC (except for usage), the code in
here is an unholy mashup of anti-patterns in erlang. Worst case example; if you
spam for 2 seconds, every message will be delayed by about 2 seconds from the
previous one. Kind of like the anti-spam on the freenode network, except theirs
is intentional.

Feel free to do whatever you want with this code, as I probably won't be maintaining
it after it _works_.

**END IMPORTANT NOTE**

How this should have been done
------------------------------

- Have two different state managers for users and channels
- Don't throw all commands at one synchronous process, this is what created the enormous bottleneck we have now
- Keep as much as you can asynchronous, as this will spawn another process that doesn't lock up the process
    - Make almost all IRC commands run asynchronously on the server; there is no reason for us to wait for a confirmation as to whether the message was sent or not. We can let the client handle this
- Rely more on the client for state management
- Pay more attention to data structure to avoid passing the entire state around to sync. processes

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
