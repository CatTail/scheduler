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
-export([start_link/0,
         start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {jobs, test}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [false], []).

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).


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
init([Test]) ->
    Jobs = case ets:file2tab("jobs") of
               {ok, Tab} ->
                   case Test of
                       true ->
                           create_table();
                       false ->
                           Tab
                   end;
               {error, _Reason} ->
                   create_table()
           end,
    % override backup job even exist on disk
    Type = "backup",
    add_or_update_job(Jobs, Type, 10),
    add_handler(Jobs, Type, self()),
    {ok, #state{jobs=Jobs, test=Test}}.

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
    add_or_update_job(Jobs, Type, Interval),
    {reply, ok, #state{jobs=Jobs}};

handle_call({remove, job, Type}, _From, #state{jobs=Jobs}) ->
    remove_job(Jobs, Type),
    {reply, ok, #state{jobs=Jobs}};

handle_call({add, handler, Type}, From, #state{jobs=Jobs}) ->
    add_handler(Jobs, Type, From),
    {reply, ok, #state{jobs=Jobs}};

handle_call({get, Type}, _From, #state{jobs=Jobs}) ->
    {reply, get_job(Jobs, Type), #state{jobs=Jobs}};

handle_call({get}, _From, #state{jobs=Jobs}) ->
    List = get_job_list(Jobs),
    Reply = lists:foldl(
               fun({Type, Job}, Result) -> Result#{ Type := Job } end,
               #{},
               List),
    {reply, Reply, #state{jobs=Jobs}}.

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
handle_info(tick, #state{jobs=Jobs}) ->
    % fetch jobs and handlers
    List = get_job_list(Jobs),
    lists:foreach(
      fun ({Type, Job}) -> 
              io:format("process job ~s\n", [Type]),
              #{interval := Interval, timelapse := Timelapse, handlers := Handlers} = Job,
              % check if handlers should notified
              case Interval > Timelapse of
                  true ->
                      update_timelapse(Jobs, Type, Timelapse + 1);
                  false ->
                      io:format("notify job ~s\n", [Type]),
                      lists:foreach(fun(Handler) -> Handler ! Type end, Handlers),
                      update_timelapse(Jobs, Type, 0)
              end
      end,
      List),
    {noreply, #state{jobs=Jobs}};

handle_info(backup, #state{jobs=Jobs}) ->
    ets:tab2file(Jobs, jobs),
    {noreply, #state{jobs=Jobs}}.

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

create_table() ->
    Jobs = ets:new(jobs, [set]),
    Jobs.

add_or_update_job(Jobs, Type, Interval) ->
    Job = case ets:lookup(Jobs, Type) of
              [] ->
                  #{timelapse => 0, handlers => []};
              [{_Type, _Job}] ->
                  _Job
          end,
    NewJob = maps:put(interval, Interval, Job),
    ets:insert(Jobs, {Type, NewJob}),
    ets:tab2file(Jobs, jobs).

update_timelapse(Jobs, Type, Timelapse) ->
    Job = ets:lookup_element(Jobs, Type, 2),
    NewJob = maps:put(timelapse, Timelapse, Job),
    % dont persist to disk on every timelapse change
    ets:insert(Jobs, {Type, NewJob}).

remove_job(Jobs, Type) ->
    ets:delete(Jobs, Type),
    ets:tab2file(Jobs, jobs).

add_handler(Jobs, Type, Handler) ->
    Job = ets:lookup_element(Jobs, Type, 2),
    Handlers = sets:from_list(maps:get(handlers, Job)),
    NewHandlers = sets:to_list(sets:add_element(Handler, Handlers)),
    NewJob = maps:put(handler, NewHandlers, Job),
    ets:insert(Jobs, {Type, NewJob}),
    ets:tab2file(Jobs, jobs).

get_job(Jobs, Type) ->
    ets:lookup_element(Jobs, Type, 2).

get_job_list(Jobs) ->
    MatchSpec = ets:fun2ms(fun({Type, Job}) -> {Type, Job} end),
    ets:select(Jobs, MatchSpec).
