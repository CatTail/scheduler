%%%-------------------------------------------------------------------
%%% @author zhongchiyu
%%% @copyright (C) 2017, zhongchiyu
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-27 15:15:28.292267
%%%-------------------------------------------------------------------
-module(scheduler_manager).

-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(INTERVAL, 1000).

-record(state, {jobs}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    erlang:send_after(?INTERVAL, self(), ping),
    Jobs = case ets:file2tab("jobs") of
               {ok, Tab} ->
                   Tab;
               {error, _Reason} ->
                   Tab = ets:new(jobs, [set]),
                   % TODO: add backup job
                   Tab
           end,
    {ok, #state{jobs=Jobs}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({put, job, Type, Interval}, _From, #state{jobs=Jobs}) ->
    Job = case ets:lookup(Jobs, Type) of
              [] ->
                  #{timelapse => 0, handlers => []};
              [{_Type, _Job}] ->
                  _Job
          end,
    NewJob = maps:put(interval, Interval, Job),
    ets:insert(Jobs, {Type, NewJob}),
    ets:tab2file(Jobs, jobs),
    {reply, ok, #state{jobs=Jobs}};

handle_call({remove, job, Type}, _From, #state{jobs=Jobs}) ->
    ets:delete(Jobs, Type),
    ets:tab2file(Jobs, jobs),
    {reply, ok, #state{jobs=Jobs}};

handle_call({add, handler, Type}, From, #state{jobs=Jobs}) ->
    Job = ets:lookup_element(Jobs, Type, 2),
    Handlers = sets:from_list(maps:get(handlers, Job)),
    NewHandlers = sets:to_list(sets:add_element(From, Handlers)),
    NewJob = maps:put(handler, NewHandlers, Job),
    ets:insert(Jobs, {Type, NewJob}),
    ets:tab2file(Jobs, jobs),
    {reply, ok, #state{jobs=Jobs}};

handle_call({get, Type}, _From, #state{jobs=Jobs}) ->
    Job = ets:lookup_element(Jobs, Type, 2),
    {reply, Job, #state{jobs=Jobs}};

handle_call({get}, _From, #state{jobs=Jobs}) ->
    MatchSpec = ets:fun2ms(fun({Type, Job}) -> {Type, Job} end),
    Matchs = ets:select(Jobs, MatchSpec),
    Result = lists:foldl(
               fun({Type, Job}, Result) -> 
                       Result#{ Type := Job }
               end,
               #{},
               Matchs),
    {reply, Result, #state{jobs=Jobs}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(ping, State) ->
    Start = erlang:system_time(),

    % fetch jobs and handlers
    % do some task
    io:format("do something\n"),
    timer:sleep(50),

    End = erlang:system_time(),
    Comsumed = erlang:list_to_integer(erlang:float_to_list((End - Start) / 1000000, [{decimals,0}])),
    io:format("Consumed ~B \n", [Comsumed]),
    if
        Comsumed >= ?INTERVAL ->
            self() ! ping;
        true ->
            erlang:send_after(?INTERVAL - Comsumed, self(), ping)
    end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

