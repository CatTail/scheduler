-module(scheduler_clock_tests).

-include_lib("eunit/include/eunit.hrl").

scheduler_clock_test_() ->
    {foreach, fun setup/0, fun cleanup/1,
     [
      fun send_message_on_interval/1,
      fun server_is_alive/1
     ]}.

setup() ->
    process_flag(trap_exit, true),
    {ok, ReceiverPid} = scheduler_receiver:start_link(),
    {ok, Pid} = scheduler_clock:start_link([ReceiverPid, 10]),
    {Pid, ReceiverPid}.

cleanup({Pid, ReceiverPid}) ->
    exit(Pid, kill),
    exit(ReceiverPid, kill),
    ?assertEqual(false, is_process_alive(Pid)),
    ?assertEqual(false, is_process_alive(ReceiverPid)),
    ok.

server_is_alive({Pid, ReceiverPid}) ->
    fun() ->
            ?assertEqual(true, is_process_alive(Pid)),
            ?assertEqual(true, is_process_alive(ReceiverPid)),
            ok
    end.

send_message_on_interval({_Pid, ReceiverPid}) ->
    fun() ->
            % FIXME: why directly sleep(100) dont have 10 messages?
            ?assertEqual(0, length(gen_server:call(ReceiverPid, dump))),
            timer:sleep(10),
            ?assertEqual(1, length(gen_server:call(ReceiverPid, dump))),
            timer:sleep(10),
            ?assertEqual(2, length(gen_server:call(ReceiverPid, dump))),
            timer:sleep(10),
            ?assertEqual(3, length(gen_server:call(ReceiverPid, dump))),
            timer:sleep(10),
            ?assertEqual(4, length(gen_server:call(ReceiverPid, dump))),
            timer:sleep(10),
            ?assertEqual(5, length(gen_server:call(ReceiverPid, dump))),
            timer:sleep(10),
            ?assertEqual(6, length(gen_server:call(ReceiverPid, dump))),
            timer:sleep(10),
            ?assertEqual(7, length(gen_server:call(ReceiverPid, dump))),
            timer:sleep(10),
            ?assertEqual(8, length(gen_server:call(ReceiverPid, dump))),
            timer:sleep(10),
            ?assertEqual(9, length(gen_server:call(ReceiverPid, dump))),
            timer:sleep(10),
            ?assertEqual(10, length(gen_server:call(ReceiverPid, dump))),
            ok
    end.
