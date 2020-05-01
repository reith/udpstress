-module(reporter).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         child_spec/0,
         collect_report/4,
         collect_report/6
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-define(REPORT_INTERVAL, 10000).

-record(state, {last_recv_pkts=0, last_recv_size=0,
                last_sent_pkts=0, last_sent_size=0,
                last_acked_pkts=undefined, last_acked_size=undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

collect_report(RecvPkts, RecvSize, SentPkts, SentSize) ->
  ?SERVER ! {collect, RecvPkts, RecvSize, SentPkts, SentSize, undefined, undefined}.

collect_report(RecvPkts, RecvSize, SentPkts, SentSize, AckedPkts, AckedSize) ->
  ?SERVER ! {collect, RecvPkts, RecvSize, SentPkts, SentSize, AckedPkts, AckedSize}.


child_spec() ->
  #{ start => {?MODULE, start_link, []} }.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(report, _State = #state{last_recv_pkts=RecvPkts, last_recv_size=RecvSize,
                                    last_sent_pkts=SentPkts, last_sent_size=SentSize,
                                    last_acked_pkts=AckedPkts, last_acked_size=AckedSize
                                   }) ->
  {NotAckedPktS, AckRateS} = case AckedPkts of
    undefined ->
      {"", ""};
    _ when is_integer(AckedPkts) ->
      {
       io_lib:format(" lost_or_late_pkts: ~b", [erlang:abs(SentPkts-AckedPkts)]),
       io_lib:format(" ack: ~.2f Mbit/s", [AckedSize * 8 / (?REPORT_INTERVAL * 1.0e3)])
      }
  end,
  lager:notice("~p: recv_pkts: ~b recv_size: ~b sent_pkts: ~b sent_size: ~b~s recv: ~.2f Mbit/s send: ~.2f Mbit/s~s",
               [erlang:system_time(seconds),
                RecvPkts, RecvSize, SentPkts, SentSize, NotAckedPktS,
                RecvSize * 8 / (?REPORT_INTERVAL * 1.0e3),
                SentSize * 8 / (?REPORT_INTERVAL * 1.0e3),
                AckRateS
               ]),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  {noreply, #state{}};
handle_info({collect, NRecvPkts, NRecvSize, NSentPkts, NSentSize, NAckedPkts, NAckedSize},
            State = #state{last_recv_pkts = RecvPkts, last_recv_size = RecvSize,
                           last_sent_pkts = SentPkts, last_sent_size = SentSize,
                           last_acked_pkts = AckedPkts, last_acked_size = AckedSize
                          }) ->
  NState = State#state{last_recv_pkts = RecvPkts + NRecvPkts,
                        last_recv_size = RecvSize + NRecvSize,
                        last_sent_pkts = SentPkts + NSentPkts,
                        last_sent_size = SentSize + NSentSize},
  case {NAckedPkts, AckedPkts} of
    {undefined, _} -> {noreply, NState};
    {N, undefined} when is_integer(N) ->
      {noreply, NState#state{last_acked_pkts = NAckedPkts, last_acked_size = NAckedSize }};
    {N, _} when is_integer(N) ->
      {noreply, NState#state{last_acked_pkts = AckedPkts + NAckedPkts,
                             last_acked_size = AckedSize + NAckedSize}}
  end;
handle_info(Info, State) ->
  lager:warning("Got unhandled message ~w", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
