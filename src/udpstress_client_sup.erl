-module('udpstress_client_sup').

-behaviour(supervisor).

%% API
-export([start_link/1, add_server/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

add_server(PortNumber) ->
  supervisor:start_child(?MODULE, [#{port => PortNumber}]).

start_link(SupArgs) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, SupArgs).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(#{
        client_type   := Client,
        server_addr   := ServerAddr,
        start_port    := StartPort,
        end_port      := EndPort,
        send_interval := Interval
     }) when is_integer(StartPort), is_integer(EndPort)
->
  Clients = [
    Client:child_spec(ServerAddr, Port, Interval) || Port <- lists:seq(StartPort, EndPort)
  ],
  Report = maps:put(id, reporter, reporter:child_spec()),
  {ok, { {one_for_one , 0, 1}, [ Report | Clients ] } }.

%%====================================================================
%% Internal functions
%%====================================================================
