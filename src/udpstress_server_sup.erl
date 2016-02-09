-module('udpstress_server_sup').

-behaviour(supervisor).

%% API
-export([start_link/2, add_server/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
add_server(PortNumber) ->
  supervisor:start_child(?MODULE, [#{port => PortNumber}]).

start_link(StartPort, EndPort) when is_integer(StartPort), is_integer(EndPort) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [StartPort, EndPort]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([StartPort, EndPort]) ->
    Servers = [
	       server:child_spec(P) || P <- lists:seq(StartPort, EndPort)
	      ],
    Report = maps:put(id, reporter, reporter:child_spec()),
    lager:notice("servers are ~p", [Servers]),
    {ok, { {one_for_one , 0, 1}, [Report | Servers] } }.

%%====================================================================
%% Internal functions
%%====================================================================
