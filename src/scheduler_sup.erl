%%%-------------------------------------------------------------------
%% @doc scheduler top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(scheduler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Jobs = case ets:file2tab("jobs") of
               {ok, Tab} ->
                   Tab;
               {error, _Reason} ->
                   ets:new(scheduler_jobs, [set, public])
           end,

    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
                  #{id => scheduler_clock,
                    start => {scheduler_clock, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [scheduler_clock]},
                  #{id => scheduler_manager,
                    start => {scheduler_manager, start_link, [[Jobs]]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [scheduler_manager]},
                  #{id => scheduler_backup,
                    start => {scheduler_backup, start_link, [[Jobs]]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [scheduler_backup]}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
