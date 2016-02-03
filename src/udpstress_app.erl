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
start_app_from_args(["client", StartPort, EndPort]) ->
  start_app_from_args(["client", "127.0.0.1", StartPort, EndPort]);
start_app_from_args(["client", Server, StartPort, EndPort]) ->
  RemoteAddr = case inet:parse_address(Server) of
    {ok, IP} -> IP;
    _ -> case inet:gethostbyname(Server) of
          {ok, {hostent, _, _, _, _, [IP | _]}} -> IP
         end
  end,
  udpstress_client_sup:start_link(RemoteAddr,
                                  erlang:list_to_integer(StartPort),
                                  erlang:list_to_integer(EndPort));
start_app_from_args(["server", StartPort, EndPort]) ->
  udpstress_server_sup:start_link(erlang:list_to_integer(StartPort),
                                  erlang:list_to_integer(EndPort));
start_app_from_args(Args) ->
  lager:error("invalid invokation: ~p~n", [lists:flatten(Args)]),
  lager:error("usage: client [server_address] start_port end_port~n"),
  lager:error("       server start_port end_port~n"),
  init:stop().
