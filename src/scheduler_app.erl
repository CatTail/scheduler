%%%-------------------------------------------------------------------
%% @doc scheduler public API
%% @end
%%%-------------------------------------------------------------------

-module(scheduler_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    scheduler_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
