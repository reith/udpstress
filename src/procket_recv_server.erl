-module(procket_recv_server).

-behaviour(udpstress_server).

-define(SERVER, ?MODULE).
-define(REPORT_INTERVAL, 1000).
-define(ASYNC_RETRY_INTERVAL, 10).

-include("udpstress_server.hrl").

%%%===================================================================
%%% udpstress_server behaviour
%%%===================================================================

-export([start_link/1, process_msg/2]).

start_link(Args = #{port := PortNumber}) ->
  Name = erlang:list_to_atom(
          lists:flatten(io_lib:format("~s_~p",
                                      [atom_to_list(?MODULE), PortNumber]))),
  register(Name, Pid = spawn_link(fun() -> start_loop(Args) end)),
  {ok, Pid}.

process_msg({udp, Socket, SockAddr, Data},
            State = #state{socket = Socket, sent_pkts = SentPkts, recv_pkts = RecvPkts,
                           sent_size = SentSize, recv_size = RecvSize}) ->
  PktSize = size(Data),
  ok = procket:sendto(Socket, <<"ack", PktSize:16>>, 0, SockAddr),
  State#state{sent_pkts = SentPkts + 1, recv_pkts = RecvPkts + 1,
              recv_size = RecvSize + PktSize, sent_size = SentSize + 5}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_loop(#{port := PortNumber}) ->
  {ok, Fd} = procket:open(PortNumber,
                          [{protocol, udp}, {type, dgram}, {family, inet}]),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  loop(#state{socket = Fd}).

loop(State = #state{socket = Socket}) ->
  receive 
    report ->
      loop(udpstress_server:report(State))
    after 0 ->
      ok
  end,
  case procket:recvfrom(Socket, 1500, 0, 16) of
    {ok, Data, SockAddr} ->
      loop(process_msg({udp, Socket, SockAddr, Data}, State));
    {error, eagain} ->
      receive after ?ASYNC_RETRY_INTERVAL ->
        loop(State)
      end
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
