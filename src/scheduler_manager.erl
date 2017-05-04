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
-include_lib("eunit/include/eunit.hrl").


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

-record(state, {jobs, handlers, backup}).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [true], []).

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
init([Backup]) ->
    Jobs = case ets:file2tab("jobs") of
               {ok, Tab} ->
                   case Backup of
                       true ->
                           Tab;
                       false ->
                           create_table()
                   end;
               {error, _Reason} ->
                   create_table()
           end,
    % override backup job even exist on disk
    Type = "backup",
    State = #state{jobs = Jobs, handlers = #{}, backup = Backup},
    TmpState = add_or_update_job(State, Type, 10),
    NewState = add_handler(TmpState, Type, self()),
    {ok, NewState}.

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
handle_call({put, job, Type, Interval}, _From, State) ->
    NewState = add_or_update_job(State, Type, Interval),
    {reply, ok, NewState};

handle_call({remove, job, Type}, _From, State) ->
    NewState = remove_job(State, Type),
    {reply, ok, NewState};

handle_call({add, handler, Type, Handler}, _From, State) ->
    NewState = add_handler(State, Type, Handler),
    {reply, ok, NewState};

handle_call({get, Type}, _From, State) ->
    {reply, get_job(State, Type), State};

handle_call({get}, _From, State) ->
    {reply, get_job_map(State), State}.

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
handle_info(tick, State) ->
    % fetch jobs and handlers
    maps:fold(
      fun (Type, Job, _Acc) -> 
              ?debugFmt("process ~s", [Type]),
              #{interval := Interval, timelapse := Timelapse, handlers := Handlers} = Job,
              % check if handlers should notified
              NewTimelapse = Timelapse + 1,
              case Interval > NewTimelapse of
                  true ->
                      update_timelapse(State, Type, NewTimelapse);
                  false ->
                      ?debugFmt("notify ~s", [Type]),
                      Length = length(Handlers),
                      case Length > 0 of
                          true ->
                              Index = rand:uniform(Length),
                              Handler = lists:nth(Index, Handlers),
                              ?debugFmt("execute ~s", [Type]),
                              Handler ! erlang:list_to_atom(Type);
                          false ->
                              % TODO: notify user missing handlers
                              ?debugFmt("~s don't have handlers", [Type]),
                              false
                      end,
                      update_timelapse(State, Type, 0)
              end
      end,
      ok,
      get_job_map(State)),
    {noreply, State};

handle_info(backup, State) ->
    backup_jobs(State),
    {noreply, State};

handle_info({'DOWN', _MonitorRef, process, Handler, _Info}, State) ->
    ?debugVal(Handler),
    NewState = remove_handler(State, Handler),
    {noreply, NewState}.

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

add_or_update_job(State, Type, Interval) ->
    Jobs = State#state.jobs,
    Job = case ets:lookup(Jobs, Type) of
              [] ->
                  #{timelapse => 0};
              [{_Type, _Job}] ->
                  _Job
          end,
    NewJob = Job#{interval => Interval},
    ets:insert(Jobs, {Type, NewJob}),
    backup_jobs(State).

update_timelapse(State, Type, Timelapse) ->
    Jobs = State#state.jobs,
    Job = ets:lookup_element(Jobs, Type, 2),
    NewJob = Job#{timelapse => Timelapse},
    % dont persist to disk on every timelapse change
    ets:insert(Jobs, {Type, NewJob}),
    State.

remove_job(State, Type) ->
    Jobs = State#state.jobs,
    ets:delete(Jobs, Type),
    backup_jobs(State).

add_handler(State, Type, Handler) ->
    erlang:monitor(process, Handler),
    Handlers = State#state.handlers,
    TypeHandlers = sets:from_list(maps:get(Type, Handlers, [])),
    NewTypeHandlers = sets:to_list(sets:add_element(Handler, TypeHandlers)),
    NewHandlers = Handlers#{ Type => NewTypeHandlers },
    backup_jobs(State#state{handlers=NewHandlers}).

remove_handler(State, Handler) ->
    Handlers = State#state.handlers,
    NewHandlers = maps:fold(
      fun(Type, TypeHandlers, NewHandlers) ->
              NewTypeHandlers = lists:delete(Handler, TypeHandlers),
              NewHandlers#{ Type => NewTypeHandlers }
      end,
      #{},
      Handlers),
    backup_jobs(State#state{handlers=NewHandlers}).

get_job(State, Type) ->
    Job = ets:lookup_element(State#state.jobs, Type, 2),
    TypeHandlers = maps:get(Type, State#state.handlers, []),
    Job#{ handlers => TypeHandlers }.

get_job_map(State) ->
    MatchSpec = ets:fun2ms(fun({Type, Job}) -> {Type, Job} end),
    Jobs = ets:select(State#state.jobs, MatchSpec),
    lists:foldl(
      fun({Type, Job}, Acc) ->
              TypeHandlers = maps:get(Type, State#state.handlers, []),
              NewJob = Job#{ handlers => TypeHandlers },
              Acc#{ Type => NewJob }
      end,
      #{},
      Jobs).

backup_jobs(State) ->
    case State#state.backup of
        true -> 
            ets:tab2file(State#state.jobs, "jobs");
        false ->
            false
    end,
    State.
