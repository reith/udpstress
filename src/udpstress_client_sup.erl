-module('udpstress_client_sup').

-behaviour(supervisor).

%% API
-export([start_link/3, add_server/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

add_server(PortNumber) ->
  supervisor:start_child(?MODULE, [#{port => PortNumber}]).

start_link(Server, StartPort, EndPort)
  when is_integer(StartPort), is_integer(EndPort)
->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Server, StartPort, EndPort]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Server, StartPort, EndPort]) ->
  Clients = [
   client:child_spec(Server, Port) || Port <- lists:seq(StartPort, EndPort)
  ],
  Report = maps:put(id, reporter, reporter:child_spec()),
  {ok, { {one_for_one , 0, 1}, [ Report | Clients] } }.

%%====================================================================
%% Internal functions
%%====================================================================
