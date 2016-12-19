-module(procket_client).

-behaviour(udpstress_client).

-define(REPORT_INTERVAL, 1000).

-include("udpstress_client.hrl").

%%%===================================================================
%%% udpstress_client behaviour
%%%===================================================================

-export([ start_link/2, send_data/2 ]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Args) ->
  Pid = spawn_link(fun() -> start_1(Args) end),
  register(Name, Pid),
  {ok, Pid}.

send_data(Data, #state{socket = Socket, remote_addr = Addr, remote_port = Port,
                       not_acked_pkts = NotAckeds} = State) ->
    gen_udp:send(Socket, Addr, Port, Data),
    State#state{not_acked_pkts = NotAckeds + 1}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_1(Args = #{addr := Addr, port := PortNumber}) ->
  {ok, Fd} = procket:open(0, [{protocol, udp}, {family, inet}, {type, dgram}]),
  {ok, Socket} = gen_udp:open(0, [{fd, Fd}, binary, {active, once}]),
  State = #state{socket = Socket, remote_addr = Addr, remote_port = PortNumber},
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  inet:setopts(Socket, [{active, once}]),
  case maps:get(send_interval, Args) of
    undefined ->
      loop(udpstress_client:send(?MODULE, State));
    Interval ->
      timer:send_interval(Interval, ping),
      loop(State#state{send_interval = Interval})
  end.

loop(#state{socket = Socket, recv_pkts = RecvPkts, recv_size = RecvSize,
	          sent_pkts = SentPkts, sent_size = SentSize} = State) ->
  receive
    {udp, Socket, _FromIP, _FromPort, <<"ack", Size:16>>} ->
      inet:setopts(Socket, [{active, once}]),
      NewState = State#state{sent_pkts = SentPkts + 1, sent_size = SentSize + Size,
                             recv_pkts = RecvPkts + 1, recv_size = RecvSize + 5},
      loop(case State#state.send_interval of
            undefined ->udpstress_client:send(?MODULE, NewState);
            _ -> NewState
           end);
    ping ->
      loop(udpstress_client:send(?MODULE, State));
    report ->
      loop(udpstress_client:report(State));
    Msg ->
      lager:warning("got unknown message ~p", [Msg])
  end.
