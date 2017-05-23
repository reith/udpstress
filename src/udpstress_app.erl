%%%-------------------------------------------------------------------
%% @doc mylib public API
%% @end
%%%-------------------------------------------------------------------

-module('udpstress_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  start_app_from_args(init:get_plain_arguments()).

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_app_from_args([StartScriptAction | Rest]) when
  StartScriptAction =:= "start";
  StartScriptAction =:= "console";
  StartScriptAction =:= "foreground"
->
  start_app_from_args(Rest);

start_app_from_args([ClientType, Server, StartPort, EndPort])
  when
    ClientType == "plain_client";
    ClientType == "genserver_client";
    ClientType == "procket_client";
    ClientType == "procket_send_client"
->
  start_app_from_args([ClientType, Server, "-i", undefined, StartPort, EndPort]);
start_app_from_args([ClientType, Server, "-i", SendInterval, StartPort, EndPort]) ->
 RemoteAddr = case inet:parse_address(Server) of
    {ok, IP} -> IP;
    _ -> case inet:gethostbyname(Server) of
          {ok, {hostent, _, _, _, _, [IP | _]}} -> IP
         end
  end,
  udpstress_client_sup:start_link(#{
                                    client_type   => list_to_atom(ClientType),
                                    server_addr   => RemoteAddr,
                                    start_port    => list_to_integer(StartPort),
                                    end_port      => list_to_integer(EndPort),
                                    send_interval => if SendInterval =/= undefined -> list_to_integer(SendInterval) ; true -> SendInterval end
                                  });
start_app_from_args([ServerType, StartPort, EndPort])
  when
    ServerType == "plain_server";
    ServerType == "genserver_server";
    ServerType == "procket_server";
    ServerType == "procket_recv_server"
->
  udpstress_server_sup:start_link(#{
                                    server_type => list_to_atom(ServerType),
                                    start_port => list_to_integer(StartPort),
                                    end_port => list_to_integer(EndPort)
                                  });
start_app_from_args(Args) ->
  lager:error("invalid invocation: ~p~n", [lists:flatten(Args)]),
  lager:error("usage: <genserver_client | plain_client | procket_client | procket_send_client> [server_address] [-i send_interval] start_port end_port~n"),
  lager:error("       <genserver_server | plain_server | procket_server | procket_recv_server> start_port end_port~n"),
  init:stop().
