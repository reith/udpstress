udpstress
=====

Open UDP socket per process and feed them with data, see how much it can hadle.

Build
-----

    $ rebar3 release

    Start server to listen to 1000 ports from 12000 to 13000

    $ VMARGS_PATH="$PWD/server.args" ./_build/default/rel/udpstress/bin/udpstress foreground -extra server 12000 13000

    From another shell start client to send to those ports

    $ VMARGS_PATH="$PWD/client.args" ./_build/default/rel/udpstress/bin/udpstress foreground  -extra client localhost 12000 13000
