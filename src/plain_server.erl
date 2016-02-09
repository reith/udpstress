-module(plain_server).

%% API
-export([
     start_link/1,
     child_spec/0,
     child_spec/1
    ]).

-define(SERVER, ?MODULE).
-define(REPORT_INTERVAL, 1000).

-record(state, {socket, sent_pkts=0, recv_pkts=0, sent_size=0, recv_size=0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args = #{port := PortNumber}) ->
  Name = erlang:list_to_atom(lists:flatten(io_lib:format("plain_~p",
                                                         [PortNumber]))),
  register(Name, Pid = spawn_link(fun() -> start_loop(Args) end)),
  {ok, Pid}.

child_spec() ->
  #{ id => server,
     start => {?MODULE, start_link, []},
     restart => permanent,
     modules => [?MODULE],
     type => worker
  }.

child_spec(PortNumber) ->
  #{ id =>  lists:flatten(io_lib:format("ps_~b", [PortNumber])),
     start => {?MODULE, start_link, [#{port => PortNumber}]},
     restart => permanent,
     modules => [?MODULE],
     type => worker
  }.
   
%%%===================================================================
%%% Internal functions
%%%===================================================================

start_loop(#{port := PortNumber}) ->
  {ok, Socket} = gen_udp:open(PortNumber, [binary, {active, once},
                                           {recbuf, 1048576},
                                           {sndbuf, 1048576}]),
  lager:notice("plain_listening on udp ~p~n", [PortNumber]),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  loop(#state{socket = Socket}).

loop(State) ->
  receive 
    Msg = {udp, _Socket, _FromIP, _FromPort, _Data} ->
      loop(process_udp(Msg, State));
    report ->
      loop(report(State))
  end.

report(State = #state{sent_pkts = SentPkts, recv_pkts = RecvPkts,
                      sent_size = SentSize, recv_size = RecvSize}) ->
  reporter:collect_report(RecvPkts, RecvSize, SentPkts, SentSize),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  State#state{sent_pkts=0, sent_size=0, recv_pkts=0, recv_size=0}.

process_udp({udp, Socket, FromIP, FromPort, Data},
            State = #state{socket = Socket, sent_pkts = SentPkts, recv_pkts = RecvPkts,
                           sent_size = SentSize, recv_size = RecvSize}) ->
  inet:setopts(Socket, [{active, once}]),
  PktSize = size(Data),
  gen_udp:send(Socket, FromIP, FromPort, << "ack", PktSize:16 >>),
  State#state{sent_pkts = SentPkts + 1, recv_pkts = RecvPkts + 1,
              recv_size = RecvSize + PktSize, sent_size = SentSize + 5}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
