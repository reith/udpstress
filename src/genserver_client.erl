-module(genserver_client).

-behaviour(gen_server).

%% API
-export([
     start_link/2,
     child_spec/0,
     child_spec/3
    ]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-define(SERVER, ?MODULE).
-define(REPORT_INTERVAL, 1000).

-define(PKT_SIZE, 1472).
-define(PKT_DATA, <<0:(8*?PKT_SIZE)>>).

-record(state, {socket, remote_addr, remote_port, sent_pkts=0, recv_pkts=0,
                sent_size=0, recv_size=0, acked_pkts=0, acked_size=0, send_interval}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name, Args) ->
  gen_server:start_link({local, Name}, ?MODULE, Args, []).

child_spec() ->
  #{ id => client,
     start => {?MODULE, start_link, []},
     restart => permanent,
     modules => [?MODULE],
     type => worker
  }.

child_spec(Addr, Port, Interval) ->
  ID = lists:flatten(io_lib:format("gc_~s_~b", [inet:ntoa(Addr), Port])),
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
%%% gen_server callbacks
%%%===================================================================

init(#{addr := Addr, port := PortNumber, send_interval := SendInterval}) ->
  {ok, Socket} = gen_udp:open(0, [binary, {active, once}]),
  erlang:send_after(100, self(), ping),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  {ok, #state{socket = Socket, remote_addr = Addr, remote_port = PortNumber,
              send_interval = SendInterval}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(report, State = #state{}) ->
  {noreply, udpstress_client:report(State)};
handle_info(ping, State = #state{socket = Socket, remote_addr = Addr, remote_port = Port,
                                 sent_pkts = SentPkts, sent_size = SentSize,
                                 send_interval = Interval}) ->
  gen_udp:send(Socket, Addr, Port, ?PKT_DATA),
  if
    Interval =/= undefined -> erlang:send_after(Interval, self(), ping);
    true -> ok
  end,
  {noreply, State#state{sent_pkts = SentPkts + 1, sent_size = SentSize + ?PKT_SIZE}};
handle_info({udp, Socket, _FromIP, _FromPort, <<"ack", Size:16>>},
            State = #state{socket = Socket, recv_pkts = RecvPkts, recv_size = RecvSize,
                           acked_pkts = AckedPkts, acked_size = AckedSize,
                           send_interval = Interval}) ->
  inet:setopts(Socket, [{active, once}]),
  if
    Interval =:= undefined -> self() ! ping;
    true -> ok
  end,
  {noreply, State#state{acked_pkts = AckedPkts + 1, acked_size = AckedSize + Size,
                        recv_pkts = RecvPkts + 1,recv_size = RecvSize + 5}};
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
