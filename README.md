udpstress
=====

Open UDP socket per process and feed them with data, see how much it can hadle.

Build
-----

    $ rebar3 release

    Start server to listen to 1000 ports from 12000 to 13000

    $ VMARGS_PATH="$PWD/server.args" ./_build/default/rel/udpstress/bin/udpstress foreground -extra genserver_server 12000 13000

    From another shell start client to send to those ports

    $ VMARGS_PATH="$PWD/client.args" ./_build/default/rel/udpstress/bin/udpstress foreground  -extra genserver_client localhost 12000 13000

    There are plain implementations without gen_server that can be used:

    $ VMARGS_PATH="$PWD/client.args" ./_build/default/rel/udpstress/bin/udpstress foreground  -extra plain_client localhost 12000 13000
    $ VMARGS_PATH="$PWD/client.args" ./_build/default/rel/udpstress/bin/udpstress foreground  -extra plain_server localhost 12000 13000

    There are these server modes:

    - plain_server: each process opens a sockets by gen_udp
    - genserver_server: each process is a gen_server openning its socket sockets through gen_udp
    - procket_server: each process opens a socket with procket and pushs file descriptor to gen_udp
    - procket_recv_server: each process opens a socket with procket and get datagrams by procket:recvfrom

    Similar to server modes, these client modes exist:

    - plain_client: each process opens a sockets by gen_udp
    - genserver_client: each process is a gen_server openning its socket sockets through gen_udp
    - procket_client: each process opens a socket with procket and pushs file descriptor to gen_udp
    - procket_send_client: each process opens a socket with procket and sends datagrams by procket:sendto
