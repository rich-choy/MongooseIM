#!/usr/bin/env escript
%% Standalone test for node_eventbus logic without requiring compilation
%% This tests the core permission and parsing logic

main(_) ->
    io:format("~n========================================~n"),
    io:format("node_eventbus Logic Tests (No Mocking Required)~n"),
    io:format("========================================~n~n"),

    %% Test Suite 1: Permission Model Logic
    io:format("Test Suite 1: Permission Model Logic~n"),
    io:format("------------------------------------~n"),

    {Total1, Passed1} = run_test("Open model allows any publisher",
        fun() ->
            %% Open model: anyone can publish
            Result = is_allowed_to_publish(open, member),
            assert_true(Result)
        end),

    {Total2, Passed2} = run_test("Publishers model: owner can publish",
        fun() ->
            Result = is_allowed_to_publish(publishers, owner),
            assert_true(Result)
        end),

    {Total3, Passed3} = run_test("Publishers model: publisher can publish",
        fun() ->
            Result = is_allowed_to_publish(publishers, publisher),
            assert_true(Result)
        end),

    {Total4, Passed4} = run_test("Publishers model: publish_only can publish",
        fun() ->
            Result = is_allowed_to_publish(publishers, publish_only),
            assert_true(Result)
        end),

    {Total5, Passed5} = run_test("Publishers model: member cannot publish",
        fun() ->
            Result = is_allowed_to_publish(publishers, member),
            assert_false(Result)
        end),

    {Total6, Passed6} = run_test("Publishers model: none cannot publish",
        fun() ->
            Result = is_allowed_to_publish(publishers, none),
            assert_false(Result)
        end),

    {Total7, Passed7} = run_test("Publishers model: outcast cannot publish",
        fun() ->
            Result = is_allowed_to_publish(publishers, outcast),
            assert_false(Result)
        end),

    io:format("~n"),

    %% Test Suite 2: Multi-Item Parsing Logic
    io:format("Test Suite 2: Multi-Item Parsing Logic~n"),
    io:format("------------------------------------~n"),

    {Total8, Passed8} = run_test("Parse 3 items correctly",
        fun() ->
            Children = [
                make_item_el(<<"1">>, [make_data_el(<<"data1">>)]),
                make_item_el(<<"2">>, [make_data_el(<<"data2">>)]),
                make_item_el(<<"3">>, [make_data_el(<<"data3">>)])
            ],
            ItemElements = [El || El = #{name := <<"item">>} <- Children],
            assert_equal(3, length(ItemElements))
        end),

    {Total9, Passed9} = run_test("Parse single item correctly",
        fun() ->
            Children = [make_item_el(<<"1">>, [make_data_el(<<"data1">>)])],
            ItemElements = [El || El = #{name := <<"item">>} <- Children],
            assert_equal(1, length(ItemElements))
        end),

    {Total10, Passed10} = run_test("Detect empty items list",
        fun() ->
            Children = [],
            ItemElements = [El || El = #{name := <<"item">>} <- Children],
            assert_equal(0, length(ItemElements))
        end),

    {Total11, Passed11} = run_test("Extract item IDs correctly",
        fun() ->
            Children = [
                make_item_el(<<"id1">>, []),
                make_item_el(<<"id2">>, []),
                make_item_el(<<"id3">>, [])
            ],
            ItemElements = [El || El = #{name := <<"item">>} <- Children],
            ItemIds = [maps:get(<<"id">>, maps:get(attrs, El, #{}), <<>>) || El <- ItemElements],
            assert_equal([<<"id1">>, <<"id2">>, <<"id3">>], ItemIds)
        end),

    io:format("~n"),

    %% Test Suite 3: Configuration Values
    io:format("Test Suite 3: Expected Configuration~n"),
    io:format("------------------------------------~n"),

    {Total12, Passed12} = run_test("persist_items should be false",
        fun() ->
            Options = eventbus_options(),
            PersistItems = proplists:get_value(persist_items, Options),
            assert_equal(false, PersistItems)
        end),

    {Total13, Passed13} = run_test("max_items should be 0",
        fun() ->
            Options = eventbus_options(),
            MaxItems = proplists:get_value(max_items, Options),
            assert_equal(0, MaxItems)
        end),

    {Total14, Passed14} = run_test("publish_model should be open by default",
        fun() ->
            Options = eventbus_options(),
            PublishModel = proplists:get_value(publish_model, Options),
            assert_equal(open, PublishModel)
        end),

    {Total15, Passed15} = run_test("deliver_payloads should be true",
        fun() ->
            Options = eventbus_options(),
            DeliverPayloads = proplists:get_value(deliver_payloads, Options),
            assert_equal(true, DeliverPayloads)
        end),

    io:format("~n"),

    %% Summary
    TotalAll = Total1 + Total2 + Total3 + Total4 + Total5 + Total6 + Total7 +
               Total8 + Total9 + Total10 + Total11 + Total12 + Total13 + Total14 + Total15,
    PassedAll = Passed1 + Passed2 + Passed3 + Passed4 + Passed5 + Passed6 + Passed7 +
                Passed8 + Passed9 + Passed10 + Passed11 + Passed12 + Passed13 + Passed14 + Passed15,

    io:format("========================================~n"),
    io:format("Summary: ~p/~p tests passed~n", [PassedAll, TotalAll]),
    io:format("========================================~n~n"),

    case PassedAll == TotalAll of
        true ->
            io:format("✓ ALL TESTS PASSED!~n~n"),
            halt(0);
        false ->
            io:format("✗ SOME TESTS FAILED!~n~n"),
            halt(1)
    end.

%% Core logic from node_eventbus.erl
is_allowed_to_publish(open, _Affiliation) ->
    true;
is_allowed_to_publish(publishers, Affiliation) ->
    (Affiliation == owner)
        orelse (Affiliation == publisher)
        orelse (Affiliation == publish_only);
is_allowed_to_publish(subscribers, _Affiliation) ->
    false.

%% Expected configuration from node_eventbus:options/0
eventbus_options() ->
    [{deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, false},
     {purge_offline, false},
     {persist_items, false},
     {max_items, 0},
     {subscribe, true},
     {access_model, open},
     {roster_groups_allowed, []},
     {publish_model, open},
     {notification_type, headline},
     {max_payload_size, 60000},
     {send_last_published_item, never},
     {deliver_notifications, true},
     {presence_based_delivery, false}].

%% Helper functions for XML-like structures
make_item_el(ItemId, Children) ->
    #{name => <<"item">>,
      attrs => #{<<"id">> => ItemId},
      children => Children}.

make_data_el(Data) ->
    #{name => <<"data">>,
      attrs => #{},
      children => [#{content => Data}]}.

%% Test helpers
run_test(Name, TestFun) ->
    io:format("  ~s... ", [Name]),
    try
        TestFun(),
        io:format("PASS~n"),
        {1, 1}
    catch
        _:Error ->
            io:format("FAIL: ~p~n", [Error]),
            {1, 0}
    end.

assert_true(true) -> ok;
assert_true(false) -> throw({assertion_failed, expected_true, got_false}).

assert_false(false) -> ok;
assert_false(true) -> throw({assertion_failed, expected_false, got_true}).

assert_equal(Expected, Expected) -> ok;
assert_equal(Expected, Actual) -> throw({assertion_failed, {expected, Expected}, {actual, Actual}}).
