-module(udpstress_server).

%% API
-export([child_spec/1, child_spec/2, report/1]).

-define(REPORT_INTERVAL, 1000).

-include("udpstress_server.hrl").

%%%===================================================================
%%% Behaviour callbacks
%%%===================================================================

-callback
start_link(Args :: #{port => _PortNumber}) ->
  {ok, Pid :: pid()}.

-callback
process_msg(Msg :: tuple(), State :: #state{}) ->
  Result :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec child_spec(Module :: atom()) -> supervisor:child_spec().
child_spec(Module) ->
  #{ id => server,
     start => {Module, start_link, []},
     restart => permanent,
     modules => [Module],
     type => worker
  }.

-spec child_spec(Module :: atom(), PortNumber :: integer()) -> supervisor:child_spec().
child_spec(Module, PortNumber) ->
  #{ id =>  lists:flatten(io_lib:format("~s~b", [atom_to_list(Module), PortNumber])),
     start => {Module, start_link, [#{port => PortNumber}]},
     restart => permanent,
     modules => [Module],
     type => worker
  }.

-spec report(State :: #state{}) -> Result :: #state{}.
report(#state{sent_pkts = SentPkts, recv_pkts = RecvPkts, sent_size = SentSize,
              recv_size = RecvSize} = State) ->
  reporter:collect_report(RecvPkts, RecvSize, SentPkts, SentSize),
  erlang:send_after(?REPORT_INTERVAL, self(), report),
  State#state{sent_pkts=0, sent_size=0, recv_pkts=0, recv_size=0}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
