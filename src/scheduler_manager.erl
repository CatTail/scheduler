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
    Jobs = case Backup of
               true ->
                   case ets:file2tab("jobs") of
                       {ok, Tab} ->
                           Tab;
                       {error, _Reason} ->
                           create_table()
                   end;
               false ->
                   create_table()
           end,
    {ok, #state{jobs = Jobs, handlers = #{}, backup = Backup}}.

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
    {reply, {ok}, NewState};

handle_call({remove, job, Type}, _From, State) ->
    NewState = remove_job(State, Type),
    {reply, {ok}, NewState};

handle_call({add, handler, Type, Handler}, _From, State) ->
    case is_job_exit(State, Type) of
        true ->
            NewState = add_handler(State, Type, Handler),
            {reply, {ok}, NewState};
        false ->
            {reply, {notfound}, State}
    end;

handle_call({get, Type}, _From, State) ->
    case is_job_exit(State, Type) of
        true ->
            {reply, {ok, get_job(State, Type)}, State};
        false ->
            {reply, {notfound}, State}
    end;

handle_call({get}, _From, State) ->
    {reply, {ok, get_job_map(State)}, State}.

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
              #{interval := Interval, timelapse := Timelapse} = Job,
              % check if handlers should notified
              NewTimelapse = Timelapse + 1,
              case Interval > NewTimelapse of
                  true ->
                      update_timelapse(State, Type, NewTimelapse);
                  false ->
                      ?debugFmt("notify ~s", [Type]),
                      send_message(State, Type),
                      update_timelapse(State, Type, 0)
              end
      end,
      ok,
      get_job_map(State)),
    {noreply, State};

handle_info({'DOWN', _MonitorRef, process, Handler, _Info}, State) ->
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
    Jobs = ets:new(scheduler_jobs, [set, public, named_table]),
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
            % FIXME: how to fix backup and manager deps?
            ets:tab2file(State#state.jobs, "jobs"),
            %send_message(State, "backup"),
            true;
        false ->
            false
    end,
    State.

is_job_exit(State, Type) ->
    case ets:lookup(State#state.jobs, Type) of
        [] ->
            false;
        [{_Type, _Job}] ->
            true
    end.

% send messages to one of the handlers
send_message(State, Type) ->
    Handlers = State#state.handlers,
    TypeHandlers = maps:get(Type, Handlers, []),
    Length = length(TypeHandlers),
    case Length > 0 of
        true ->
            Index = rand:uniform(Length),
            Handler = lists:nth(Index, TypeHandlers),
            ?debugFmt("send message ~s", [Type]),
            Handler ! erlang:list_to_atom(Type);
        false ->
            % TODO: notify user missing handlers
            ?debugFmt("~s don't have handlers", [Type]),
            false
    end.
