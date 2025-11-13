#!/usr/bin/env escript
%%! -pa /tmp

-define(PASS, "\033[32mPASS\033[0m").
-define(FAIL, "\033[31mFAIL\033[0m").

main(_) ->
    io:format("~n========================================~n"),
    io:format("node_eventbus_SUITE Test Runner~n"),
    io:format("========================================~n~n"),

    %% Load the test module
    code:add_patha("/tmp"),
    case code:ensure_loaded(node_eventbus_SUITE) of
        {module, node_eventbus_SUITE} ->
            io:format("✓ Test module loaded~n~n"),
            run_tests();
        {error, Reason} ->
            io:format("✗ Failed to load test module: ~p~n", [Reason]),
            halt(1)
    end.

run_tests() ->
    %% Get all test groups
    Groups = node_eventbus_SUITE:groups(),
    AllTests = lists:flatmap(fun({_GroupName, _, Tests}) -> Tests end, Groups),

    %% Initialize suite
    Config = node_eventbus_SUITE:init_per_suite([]),

    %% Run each test
    Results = lists:map(fun(TestName) -> run_test(TestName, Config) end, AllTests),

    %% Cleanup
    node_eventbus_SUITE:end_per_suite(Config),

    %% Summary
    Passed = length([ok || {pass, _} <- Results]),
    Failed = length([ok || {fail, _, _} <- Results]),
    Total = Passed + Failed,

    io:format("~n========================================~n"),
    io:format("Summary: ~p/~p tests passed~n", [Passed, Total]),
    io:format("========================================~n~n"),

    case Failed of
        0 ->
            io:format("✓ ALL TESTS PASSED!~n~n"),
            halt(0);
        _ ->
            io:format("✗ ~p TESTS FAILED!~n~n", [Failed]),
            halt(1)
    end.

run_test(TestName, Config) ->
    io:format("  ~p... ", [TestName]),

    %% Init test
    TestConfig = node_eventbus_SUITE:init_per_testcase(TestName, Config),

    try
        %% Run the test
        node_eventbus_SUITE:TestName(TestConfig),
        io:format("~s~n", [?PASS]),
        {pass, TestName}
    catch
        Class:Reason:Stacktrace ->
            io:format("~s~n", [?FAIL]),
            io:format("    Error: ~p:~p~n", [Class, Reason]),
            case Stacktrace of
                [H|_] when is_tuple(H) ->
                    {M, F, A, Info} = case tuple_to_list(H) of
                                          [M0, F0, A0, Info0] -> {M0, F0, A0, Info0};
                                          [M0, F0, A0] -> {M0, F0, A0, []};
                                          _ -> {unknown, unknown, unknown, []}
                                      end,
                    io:format("    Location: ~p:~p/~p~n", [M, F, A]),
                    case proplists:get_value(line, Info, undefined) of
                        undefined -> ok;
                        Line -> io:format("    Line: ~p~n", [Line])
                    end;
                _ -> ok
            end,
            {fail, TestName, Reason}
    after
        %% Cleanup test
        node_eventbus_SUITE:end_per_testcase(TestName, TestConfig)
    end.
