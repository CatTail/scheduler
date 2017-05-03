-module(scheduler_manager_tests).

-include_lib("eunit/include/eunit.hrl").

scheduler_manager_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [
      fun monitor_handler_crash/1,
      fun add_duplicate_handler/1,
      fun add_handler/1,
      fun create_backup_job/1,
      fun remove_job/1,
      fun update_job_interval/1,
      fun create_new_job/1,
      fun server_is_alive/1
     ]}.

setup() ->
    process_flag(trap_exit, true),
    {ok, Pid} = scheduler_manager:start_link([false]),
    Pid.

cleanup(Pid) ->
    exit(Pid, kill),
    ?assertEqual(false, is_process_alive(Pid)),
    ok.

server_is_alive(Pid) ->
    fun() ->
            ?assertEqual(true, is_process_alive(Pid)),
            ok
    end.

create_new_job(Pid) ->
    fun() ->
            JobName = "job-name",
            JobInterval = 10,
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, JobInterval})),
            ?assertMatch(#{interval := JobInterval, timelapse := 0, handlers := []}, gen_server:call(Pid, {get, JobName})),
            ok
    end.

update_job_interval(Pid) ->
    fun() ->
            JobName = "job-name",
            JobInterval = 10,
            NewJobInterval = 20,
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, JobInterval})),
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, NewJobInterval})),
            ?assertMatch(#{interval := NewJobInterval, timelapse := 0, handlers := []}, gen_server:call(Pid, {get, JobName})),
            ok
    end.

remove_job(Pid) ->
    fun() ->
            JobName = "job-name",
            JobInterval = 10,
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, JobInterval})),
            ?assertMatch(#{interval := JobInterval, timelapse := 0, handlers := []}, gen_server:call(Pid, {get, JobName})),
            ?assertEqual(ok, gen_server:call(Pid, {remove, job, JobName})),
            ?assertExit({{badarg, _}, _}, gen_server:call(Pid, {get, JobName})),
            ok
    end.

create_backup_job(Pid) ->
    fun() ->
            ?assertMatch(#{interval := 10, timelapse := 0, handlers := [_]}, gen_server:call(Pid, {get, "backup"})),
            ok
    end.

add_handler(Pid) ->
    fun() ->
            JobName = "job-name",
            JobInterval = 10,
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, JobInterval})),
            ?assertEqual(ok, gen_server:call(Pid, {add, handler, JobName})),
            ?assertMatch(#{interval := 10, timelapse := 0, handlers := [_]}, gen_server:call(Pid, {get, JobName})),
            ok
    end.

add_duplicate_handler(Pid) ->
    fun() ->
            JobName = "job-name",
            JobInterval = 10,
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, JobInterval})),
            ?assertEqual(ok, gen_server:call(Pid, {add, handler, JobName})),
            ?assertEqual(ok, gen_server:call(Pid, {add, handler, JobName})),
            ?assertMatch(#{interval := 10, timelapse := 0, handlers := [_]}, gen_server:call(Pid, {get, JobName})),
            ok
    end.

monitor_handler_crash(Pid) ->
    fun() ->
            JobName = "job-name",
            AnotherJobName = "another-job-name",
            JobInterval = 10,
            ?assertEqual(ok, gen_server:call(Pid, {put, job, JobName, JobInterval})),
            ?assertEqual(ok, gen_server:call(Pid, {put, job, AnotherJobName, JobInterval})),
            Handler = spawn_link(
                        fun() -> 
                                gen_server:call(Pid, {add, handler, JobName}),
                                gen_server:call(Pid, {add, handler, AnotherJobName}),
                                receive done -> ok end
                        end),
            timer:sleep(10),
            ?assertMatch(#{interval := 10, timelapse := 0, handlers := [_]}, gen_server:call(Pid, {get, JobName})),
            ?assertMatch(#{interval := 10, timelapse := 0, handlers := [_]}, gen_server:call(Pid, {get, AnotherJobName})),
            Handler ! done,
            timer:sleep(10),
            ?assertMatch(#{interval := 10, timelapse := 0, handlers := []}, gen_server:call(Pid, {get, JobName})),
            ?assertMatch(#{interval := 10, timelapse := 0, handlers := []}, gen_server:call(Pid, {get, AnotherJobName})),
            ok
    end.
