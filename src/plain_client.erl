-module(plain_client).

%% API
-export([
	 start_link/2,
   start/1,
   child_spec/3
	]).

-define(REPORT_INTERVAL, 1000).

-define(PKT_DATA, <<0:(8*1472)>>).

-record(state, {socket, remote_addr, remote_port, sent_pkts=0, recv_pkts=0,
                sent_size=0, recv_size=0, not_acked_pkts=0, send_interval}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Args) ->
  Pid = spawn_link(fun() -> start_1(Args) end),
  register(Name, Pid),
  {ok, Pid}.

start(Args) ->
  spawn(fun() -> start_1(Args) end).

child_spec(Addr, Port, Interval) ->
  ID = lists:flatten(io_lib:format("pc_~s_~b", [inet:ntoa(Addr), Port])),
  #{
     id => ID,
     start => {?MODULE, start_link, [list_to_atom(ID),
                                     #{port => Port, addr => Addr,
                                       send_interval => Interval}]},
     restart => permanent,
     modules => [?MODULE],
     type => worker
  }.
 
%%%===================================================================
%%% Internal functions
%%%===================================================================

start_1(Args = #{addr := Addr, port := PortNumber}) ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, once}]),
  State = #state{socket = Socket, remote_addr = Addr, remote_port = PortNumber},
  timer:send_interval(?REPORT_INTERVAL, self(), report),
  inet:setopts(Socket, [{active, once}]),
  case maps:get(send_interval, Args) of
    undefined ->
      loop(ping(State));
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
            undefined -> ping(NewState);
            _ -> NewState
           end);
    ping ->
      loop(ping(State));
    report ->
      reporter:collect_report(RecvPkts, RecvSize, SentPkts, SentSize),
      loop(State#state{sent_pkts=0, sent_size=0, recv_pkts=0, recv_size=0,
           not_acked_pkts=0});
    Msg ->
      lager:warning("got unknown message ~p", [Msg])
  end.

ping(#state{socket = Socket, remote_addr = Addr, remote_port = Port,
	    not_acked_pkts = NotAckeds} = State) ->
    gen_udp:send(Socket, Addr, Port, ?PKT_DATA),
    State#state{not_acked_pkts = NotAckeds + 1}.
