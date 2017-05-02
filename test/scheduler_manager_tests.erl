-module(scheduler_manager_tests).

-include_lib("eunit/include/eunit.hrl").

scheduler_manager_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [
      fun server_is_alive/1,
      fun create_new_job/1,
      fun update_job_interval/1,
      fun remove_job/1
     ]}.

setup() ->
    process_flag(trap_exit, true),
    {ok, Pid} = scheduler_manager:start_link(),
    Pid.

cleanup(Pid) ->
    exit(Pid, kill),
    ?assertEqual(false, is_process_alive(Pid)).

server_is_alive(Pid) ->
    fun() ->
            ?assertEqual(true, is_process_alive(Pid))
    end.

create_new_job(Pid) ->
    fun() ->
            JobName = "job-name",
            JobInterval = 10,
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, JobInterval})),
            ?assertMatch(#{interval := JobInterval, timelapse := 0, handlers := []}, gen_server:call(Pid, {get, JobName}))
    end.

update_job_interval(Pid) ->
    fun() ->
            JobName = "job-name",
            JobInterval = 10,
            NewJobInterval = 20,
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, JobInterval})),
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, NewJobInterval})),
            ?assertMatch(#{interval := NewJobInterval, timelapse := 0, handlers := []}, gen_server:call(Pid, {get, JobName}))
    end.

remove_job(Pid) ->
    fun() ->
            JobName = "job-name",
            JobInterval = 10,
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, JobInterval})),
            ?assertMatch(#{interval := JobInterval, timelapse := 0, handlers := []}, gen_server:call(Pid, {get, JobName})),
            ?assertEqual(ok, gen_server:call(Pid, {remove, job, JobName})),
            ?assertExit({{badarg, _}, _}, gen_server:call(Pid, {get, JobName}))
    end.
