-module(genserver_server).

-behaviour(gen_server).

%% API
-export([
     start_link/1,
     child_spec/0,
     child_spec/1
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

-record(state, {socket, sent_pkts=0, recv_pkts=0, sent_size=0, recv_size=0}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args = #{port := PortNumber}) ->
  Name = erlang:list_to_atom(lists:flatten(io_lib:format("genserver_server_~p",
                                                         [PortNumber]))),
  gen_server:start_link({local, Name}, ?MODULE, Args, []).

child_spec() ->
  #{ id => server,
     start => {?MODULE, start_link, []},
     restart => permanent,
     modules => [?MODULE],
     type => worker
  }.

child_spec(PortNumber) ->
  #{ id =>  lists:flatten(io_lib:format("gs_~b", [PortNumber])),
     start => {?MODULE, start_link, [#{port => PortNumber}]},
     restart => permanent,
     modules => [?MODULE],
     type => worker
  }.
   
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#{port := PortNumber}) ->
  {ok, Socket} = gen_udp:open(PortNumber, [binary, {active, once},
                                           {recbuf, 1048576},
                                           {sndbuf, 1048576}]),
  lager:notice("listening on udp ~p~n", [PortNumber]),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  {ok, #state{socket = Socket}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(report, State = #state{sent_pkts = SentPkts, recv_pkts = RecvPkts,
                                   sent_size = SentSize, recv_size = RecvSize}) ->
  reporter:collect_report(RecvPkts, RecvSize, SentPkts, SentSize),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  {noreply, State#state{sent_pkts=0, sent_size=0, recv_pkts=0, recv_size=0}};
handle_info({udp, Socket, FromIP, FromPort, Data},
            State = #state{socket = Socket, sent_pkts = SentPkts, recv_pkts = RecvPkts,
                           sent_size = SentSize, recv_size = RecvSize}) ->
  inet:setopts(Socket, [{active, once}]),
  PktSize = size(Data),
  gen_udp:send(Socket, FromIP, FromPort, << "ack", PktSize:16 >>),
  {noreply, State#state{sent_pkts = SentPkts + 1, recv_pkts = RecvPkts + 1,
                        recv_size = RecvSize + PktSize, sent_size = SentSize + 5}};
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
