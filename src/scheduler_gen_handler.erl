-module(scheduler_gen_handler).

-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([
         start_link/4,
         sync_hello/1,
         async_hello/1,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {module, name, interval, modstate}).

%%%===================================================================
%%% User callback declarations
%%%===================================================================

-callback init(Args::list()) ->
    {ok, State::tuple()} |
    {ok, State::term(), Timeout::integer()} |
    ignore |
    {stop, Reason::atom()}.
-callback execute(Type::string(), State::term()) ->
    {ok, State::term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(Module::atom(), Name::string(), Interval::integer(), Args::list()) ->
    {ok, Pid::atom()} |
    ignore |
    {error, Reason::atom()}.

start_link(Mod, Name, Interval, Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Mod, Name, Interval, Args], []).

-spec sync_hello(Msg::string()) -> ok.

sync_hello(Msg) ->
    gen_server:call(?SERVER, {hello, Msg}).

-spec async_hello(Msg::string()) -> ok.

async_hello(Msg) ->
    gen_server:cast(?SERVER, {hello, Msg}).

-spec stop() -> ok.

stop() ->
    io:format("(scheduler_gen_handler) stopping~n"),
    gen_server:stop(?SERVER).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args::list()) ->
    {ok, State::tuple()} |
    {ok, State::term(), Timeout::integer()} |
    ignore |
    {stop, Reason::atom()}.

init([Mod, Name, Interval, Args]) ->
    {ok, ModState} = Mod:init(Args),
    State = #state{module=Mod, name=Name, interval=Interval, modstate=ModState},
    reconnect(State),
    {ok, State}.

-spec handle_call(Request::any(), From::pid(), State::tuple()) ->
    {reply, Reply::any(), State::tuple()} |
    {reply, Reply::any(), State::tuple(), Timeout::integer()} |
    {noreply, State::tuple()} |
    {noreply, State::tuple(), Timeout::integer()} |
    {stop, Reason::atom(), Reply::any(), State::tuple()} |
    {stop, Reason::atom(), State::tuple()}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

-spec handle_cast(Msg::any(), State::tuple()) ->
    {noreply, State::tuple()} |
    {noreply, State::tuple(), Timeout::integer()} |
    {stop, Reason::atom(), State::tuple()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info::any(), State::tuple()) ->
    {noreply, State::tuple()} |
    {noreply, State::tuple(), Timeout::integer()} |
    {stop, Reason::atom(), State::tuple()}.

handle_info({'DOWN', _MonitorRef, process, _Object, _Info}, State) ->
    reconnect(State),
    {noreply, State};

handle_info(Type, #state{module=Mod, modstate=ModState}=State) ->
    {noreply, NewModState} = Mod:execute(Type, ModState),
    {noreply, State#state{modstate=NewModState}}.

-spec terminate(Reason::atom(), State::tuple()) -> Void::any().

terminate(_Reason, _State) ->
    io:format("(terminate) stopping~n"),
    ok.

-spec code_change(OldVsn::term(), State::tuple(), Extra::term()) ->
    {ok, NewState::tuple()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

reconnect(#state{name=Name, interval=Interval}=State) ->
    case whereis(scheduler_manager) of
        undefined ->
            timer:sleep(10),
            reconnect(State);
        _ ->
            gen_server:call(scheduler_manager, {put, job, Name, Interval}),
            gen_server:call(scheduler_manager, {add, handler, Name, self()}),
            erlang:monitor(process, scheduler_manager)
    end,
    ok.
