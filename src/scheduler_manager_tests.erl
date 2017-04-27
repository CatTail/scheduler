-module(scheduler_manager_tests).
 
-include_lib("eunit/include/eunit.hrl").
 
scheduler_manager_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun server_is_alive/1,
        fun something_else/1
    ]}.
 
setup() ->
    ?debugMsg("setup"),
    process_flag(trap_exit, true),
    {ok, Pid} = scheduler_manager:start_link(),
    Pid.
 
cleanup(Pid) ->
    ?debugMsg("cleanup"),
    exit(Pid, kill),
    ?assertEqual(false, is_process_alive(Pid)).
 
server_is_alive(Pid) ->
    fun() ->
        ?assertEqual(true, is_process_alive(Pid))
    end.
 
something_else(Pid) ->
    fun() ->
        ?assertEqual(ok, gen_server:call(Pid, {put, job, "job-name", 10}))
    end.
