-module(scheduler_sup_tests).

-include_lib("eunit/include/eunit.hrl").

scheduler_sup_test_() ->
    {inorder, 
     {foreach, fun setup/0, fun cleanup/1,
      [
       fun server_is_alive/1,
       fun handle_crash_state/1
      ]
     }
    }.

setup() ->
    process_flag(trap_exit, true),
    {ok, Pid} = scheduler_sup:start_link(),
    Pid.

cleanup(Pid) ->
    exit(Pid, kill),
    ?assertEqual(false, is_process_alive(Pid)).

server_is_alive(Pid) ->
    fun() ->
            ?assertEqual(true, is_process_alive(Pid))
    end.

handle_crash_state(_Pid) ->
    fun() ->
            JobName = "job-name",
            AnotherJobName = "another-job-name",
            JobInterval = 10,

            ?assertEqual(ok, gen_server:call(scheduler_manager, {put, job, JobName, JobInterval})),
            ?assertEqual(ok, gen_server:call(scheduler_manager, {put, job, AnotherJobName, JobInterval})),
            ?assertMatch(#{interval := JobInterval, timelapse := 0, handlers := []}, gen_server:call(scheduler_manager, {get, JobName})),
            ?assertMatch(#{interval := JobInterval, timelapse := 0, handlers := []}, gen_server:call(scheduler_manager, {get, AnotherJobName})),

            exit(whereis(scheduler_manager), kill),
            % wait for child to restart
            timer:sleep(1),
            ?assertMatch(#{interval := JobInterval, timelapse := 0, handlers := []}, gen_server:call(scheduler_manager, {get, AnotherJobName})),
            % cleanup
            ?assertEqual(ok, gen_server:call(scheduler_manager, {remove, job, AnotherJobName}))
    end.
