%%%-------------------------------------------------------------------
%%% @author zhongchiyu
%%% @copyright (C) 2017, zhongchiyu
%%% @doc
%%%
%%% @end
%%% Created : 2017-05-04 17:44:41.915314
%%%-------------------------------------------------------------------
-module(scheduler_backup).

-behaviour(scheduler_gen_handler).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         execute/2]).

-define(SERVER, ?MODULE).

-record(state, {jobs}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Jobs) ->
    scheduler_gen_handler:start_link(?MODULE, "backup", 10, [Jobs]).

init([Jobs]) ->
    {ok, #state{jobs=Jobs}}.

%%%===================================================================
%%% scheduler_gen_handler callbacks
%%%===================================================================

execute(backup, State) ->
    ?debugMsg("execute backup"),
    ets:tab2file(State#state.jobs, "jobs"),
    {noreply, State}.
