-module(udpstress_client).

%% API
-export([child_spec/1, child_spec/4, report/1, send/2]).

-define(PKT_DATA, <<0:(8*1472)>>).
-define(REPORT_INTERVAL, 1000).

-include("udpstress_client.hrl").

%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

-callback
start_link(Name :: atom(), Args :: #{port := _PortNumber :: integer(), addr := Addr :: inet:hostanme()}) ->
  {ok, Pid :: pid()}.

-callback
send_data(Data :: binary(), State :: #state{}) ->
  Result :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec child_spec(Module :: atom(), Addr :: inet:hostname(), Port :: integer(), Interval :: integer()) -> Result :: supervisor:child_spec().
child_spec(Module, Addr, Port, Interval) ->
  ID = lists:flatten(io_lib:format("~s_~s_~b", [erlang:atom_to_list(Module), inet:ntoa(Addr), Port])),
  #{
     id => ID,
     start => {Module, start_link,
               [list_to_atom(ID), #{port => Port, addr => Addr,
                send_interval => Interval}]},
     restart => permanent,
     modules => [Module],
     type => worker
  }.
 
-spec child_spec(Module :: atom()) -> supervisor:child_spec().
child_spec(Module) ->
  #{ id => client,
     start => {Module, start_link, []},
     restart => permanent,
     modules => [Module],
     type => worker
  }.

-spec send(Module :: atom(), State :: #state{}) -> Result :: #state{}.
send(Module, State) ->
  Module:send_data(?PKT_DATA, State).


-spec report(State :: #state{}) -> Result :: #state{}.
report(#state{recv_pkts = RecvPkts, recv_size = RecvSize, sent_pkts = SentPkts,
              sent_size = SentSize} = State) ->
  reporter:collect_report(RecvPkts, RecvSize, SentPkts, SentSize),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  State#state{sent_pkts=0, sent_size=0, recv_pkts=0, recv_size=0,
              not_acked_pkts=0}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
