-module(procket_send_client).

-behaviour(udpstress_client).

-define(REPORT_INTERVAL, 1000).
-define(ASYNC_RETRY_INTERVAL, 10).

-include("udpstress_client.hrl").

%%%===================================================================
%%% udpstress_client behaviour
%%%===================================================================

-export([ start_link/2, send_data/3 ]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Args) ->
  Pid = spawn_link(fun() -> start_1(Args) end),
  register(Name, Pid),
  {ok, Pid}.

send_data(Data, Size, #state{socket = Socket, remote_addr = {I1,I2,I3,I4},
                       remote_port = Port,
                       sent_pkts = SentPkts, sent_size = SentSize} = State) ->
    ok = procket:sendto(Socket, Data, 0,
                        <<2:2/native-unit:8, Port:16, I1, I2, I3, I4, 0:64>>),
    State#state{sent_pkts = SentPkts + 1, sent_size = SentSize + Size}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_1(Args = #{addr := Addr, port := PortNumber}) ->
  {ok, Fd} = procket:open(0, [{protocol, udp}, {type, dgram}, {family, inet}]),
  State = #state{socket = Fd, remote_addr = Addr, remote_port = PortNumber},
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  case maps:get(send_interval, Args) of
    undefined ->
      loop(udpstress_client:send(?MODULE, State));
    Interval ->
      timer:send_interval(Interval, ping),
      loop(State#state{send_interval = Interval})
  end.

loop(#state{socket = Socket, recv_pkts = RecvPkts, recv_size = RecvSize,
	          acked_pkts = AckedPkts, acked_size = AckedSize} = State) ->
  receive
    ping ->
      loop(udpstress_client:send(?MODULE, State));
    report ->
      loop(udpstress_client:report(State));
    Msg ->
      lager:warning("got unknown message ~p", [Msg])
    after ?ASYNC_RETRY_INTERVAL ->
      case procket:recvfrom(Socket, 1500, 0, 16) of
        {error, eagain} ->
          loop(State);
        {ok, <<"ack", Size:16>>, _SockAddr} ->
          NewState = State#state{acked_pkts = AckedPkts + 1, acked_size = AckedSize + Size,
                                 recv_pkts = RecvPkts + 1, recv_size = RecvSize + 5},
          loop(case State#state.send_interval of
                 undefined ->udpstress_client:send(?MODULE, NewState);
               _ -> NewState
               end)
      end
  end.
