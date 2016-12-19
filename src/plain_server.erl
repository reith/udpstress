-module(plain_server).

-behaviour(udpstress_server).

-define(SERVER, ?MODULE).
-define(REPORT_INTERVAL, 1000).

-include("udpstress_server.hrl").

%%%===================================================================
%%% udpstress_server behaviour
%%%===================================================================

-export([start_link/1, process_msg/2]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args = #{port := PortNumber}) ->
  Name = erlang:list_to_atom(lists:flatten(
            io_lib:format("~s_~p", [atom_to_list(?MODULE), PortNumber]))),
  register(Name, Pid = spawn_link(fun() -> start_loop(Args) end)),
  {ok, Pid}.

process_msg({udp, Socket, FromIP, FromPort, Data},
            #state{socket = Socket, sent_pkts = SentPkts, recv_pkts = RecvPkts,
                   sent_size = SentSize, recv_size = RecvSize} = State) ->
  inet:setopts(Socket, [{active, once}]),
  PktSize = size(Data),
  gen_udp:send(Socket, FromIP, FromPort, << "ack", PktSize:16 >>),
  State#state{sent_pkts = SentPkts + 1, recv_pkts = RecvPkts + 1,
              recv_size = RecvSize + PktSize, sent_size = SentSize + 5}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_loop(#{port := PortNumber}) ->
  {ok, Socket} = gen_udp:open(PortNumber, [binary, {active, once},
                                           {recbuf, 1048576},
                                           {sndbuf, 1048576}]),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  loop(#state{socket = Socket}).

loop(State) ->
  receive 
    Msg = {udp, _Socket, _FromIP, _FromPort, _Data} ->
      loop(process_msg(Msg, State));
    report ->
      loop(udpstress_server:report(State))
  end.
