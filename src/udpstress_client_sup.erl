-module('udpstress_client_sup').

-behaviour(supervisor).

%% API
-export([start_link/4, add_server/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

add_server(PortNumber) ->
  supervisor:start_child(?MODULE, [#{port => PortNumber}]).

start_link(Client, ServerAddr, StartPort, EndPort)
  when is_integer(StartPort), is_integer(EndPort)
->
  supervisor:start_link({local, ?SERVER}, ?MODULE,
                        [Client, ServerAddr, StartPort, EndPort]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

% init([plain_client, ServerAddr, StartPort, EndPort]) ->
  % lists:foreach(fun(X) ->
                  % plain_client:start(#{port => X, addr => ServerAddr})
                % end, lists:seq(StartPort, EndPort)),
  % Report = maps:put(id, reporter, reporter:child_spec()),
  % {ok, { {one_for_one, 0, 1}, [Report] } };
init([Client, ServerAddr, StartPort, EndPort]) ->
  Clients = [
    Client:child_spec(ServerAddr, Port) || Port <- lists:seq(StartPort, EndPort)
  ],
  Report = maps:put(id, reporter, reporter:child_spec()),
  {ok, { {one_for_one , 0, 1}, [ Report | Clients ] } }.

%%====================================================================
%% Internal functions
%%====================================================================
