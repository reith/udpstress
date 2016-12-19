-module(genserver_server).

-behaviour(gen_server).
-behaviour(udpstress_server).

-define(SERVER, ?MODULE).
-define(REPORT_INTERVAL, 1000).

-include("udpstress_server.hrl").

%%%===================================================================
%%% udpstress_server behaviour
%%%===================================================================

-export([start_link/1, process_msg/2]).

%%%===================================================================
%%% genserver behaviour
%%%===================================================================

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%%===================================================================
%%% udpstress_server callbacks
%%%===================================================================

start_link(Args = #{port := PortNumber}) ->
  Name = erlang:list_to_atom(lists:flatten(io_lib:format("~s_~p",
                                                         [atom_to_list(?MODULE),
                                                          PortNumber]))),
  gen_server:start_link({local, Name}, ?MODULE, Args, []).

process_msg({udp, Socket, FromIP, FromPort, Data},
            #state{socket = Socket, sent_pkts = SentPkts, recv_pkts = RecvPkts,
                   sent_size = SentSize, recv_size = RecvSize} = State) ->
  inet:setopts(Socket, [{active, once}]),
  PktSize = size(Data),
  gen_udp:send(Socket, FromIP, FromPort, << "ack", PktSize:16 >>),
  State#state{sent_pkts = SentPkts + 1, recv_pkts = RecvPkts + 1,
              recv_size = RecvSize + PktSize, sent_size = SentSize + 5}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#{port := PortNumber}) ->
  {ok, Socket} = gen_udp:open(PortNumber, [binary, {active, once},
                                           {recbuf, 1048576},
                                           {sndbuf, 1048576}]),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(report, State) ->
  {noreply, udpstress_server:report(State)};
handle_info(UDP = {udp, Socket, _, _, _}, State = #state{socket = Socket}) ->
  {noreply, process_msg(UDP, State)};
handle_info(Info, State) ->
  lager:warning("received unhandled info ~p", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
