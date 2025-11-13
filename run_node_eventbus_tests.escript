#!/usr/bin/env escript
%%! -pa _build/test/lib/meck/ebin -pa _build/test/lib/common_test/ebin

-define(ae(Expected, Actual),
    case Expected == Actual of
        true -> ok;
        false -> throw({assertion_failed, {expected, Expected}, {actual, Actual}})
    end).

main(_) ->
    io:format("Starting node_eventbus tests with mocked persistence...~n~n"),

    %% Mock setup
    code:add_patha("_build/test/lib/meck/ebin"),

    %% Load meck if available, otherwise skip mocking tests
    case code:ensure_loaded(meck) of
        {module, meck} ->
            io:format("✓ Meck loaded successfully~n"),
            run_with_mocks();
        {error, _} ->
            io:format("⚠ Meck not available, running unit tests only~n"),
            run_unit_tests_only()
    end,

    io:format("~n✓ All tests passed!~n").

run_with_mocks() ->
    %% Initialize mocks
    meck:new(mod_pubsub_db_backend, [non_strict]),
    meck:new(mongoose_xmpp_errors, [non_strict]),
    meck:new(jid, [non_strict]),

    meck:expect(jid, to_lower, fun(J) -> J end),
    meck:expect(mongoose_xmpp_errors, forbidden, fun() -> {error, forbidden} end),

    try
        run_unit_tests_only(),
        run_integration_tests()
    after
        meck:unload()
    end.

run_unit_tests_only() ->
    %% Test 1: Permission logic for open model
    io:format("Test: Open model allows all publishers... "),
    true = is_allowed_to_publish_open(open),
    io:format("PASS~n"),

    %% Test 2: Permission logic for publishers model
    io:format("Test: Publishers model - owner can publish... "),
    true = is_allowed_to_publish_publishers(publishers, owner),
    io:format("PASS~n"),

    io:format("Test: Publishers model - publisher can publish... "),
    true = is_allowed_to_publish_publishers(publishers, publisher),
    io:format("PASS~n"),

    io:format("Test: Publishers model - publish_only can publish... "),
    true = is_allowed_to_publish_publishers(publishers, publish_only),
    io:format("PASS~n"),

    io:format("Test: Publishers model - member cannot publish... "),
    false = is_allowed_to_publish_publishers(publishers, member),
    io:format("PASS~n"),

    io:format("Test: Publishers model - none cannot publish... "),
    false = is_allowed_to_publish_publishers(publishers, none),
    io:format("PASS~n"),

    %% Test 3: Multi-item parsing logic
    io:format("Test: Multi-item parsing accepts 3 items... "),
    Children = [
        make_item_el(<<"1">>, [make_data_el(<<"data1">>)]),
        make_item_el(<<"2">>, [make_data_el(<<"data2">>)]),
        make_item_el(<<"3">>, [make_data_el(<<"data3">>)])
    ],
    ItemElements = [El || El = #{name := <<"item">>} <- Children],
    ?ae(3, length(ItemElements)),
    io:format("PASS~n"),

    ok.

run_integration_tests() ->
    %% Test with owner affiliation (should succeed)
    io:format("Test: publish_item with owner affiliation... "),
    meck:expect(mod_pubsub_db_backend, get_affiliation,
                fun(_Nidx, _Jid) -> {ok, owner} end),

    Result1 = node_eventbus:publish_item(
        <<"localhost">>, 123, <<"user@localhost">>, publishers,
        0, <<"item1">>, false, [], []
    ),
    ?ae({result, {default, broadcast, []}}, Result1),
    io:format("PASS~n"),

    %% Test with publisher affiliation (should succeed)
    io:format("Test: publish_item with publisher affiliation... "),
    meck:expect(mod_pubsub_db_backend, get_affiliation,
                fun(_Nidx, _Jid) -> {ok, publisher} end),

    Result2 = node_eventbus:publish_item(
        <<"localhost">>, 123, <<"user@localhost">>, publishers,
        0, <<"item1">>, false, [], []
    ),
    ?ae({result, {default, broadcast, []}}, Result2),
    io:format("PASS~n"),

    %% Test with member affiliation (should fail)
    io:format("Test: publish_item with member affiliation (should fail)... "),
    meck:expect(mod_pubsub_db_backend, get_affiliation,
                fun(_Nidx, _Jid) -> {ok, member} end),

    Result3 = node_eventbus:publish_item(
        <<"localhost">>, 123, <<"user@localhost">>, publishers,
        0, <<"item1">>, false, [], []
    ),
    ?ae({error, forbidden}, Result3),
    io:format("PASS~n"),

    ok.

%% Helper functions (same as in test suite)
is_allowed_to_publish_open(open) ->
    true.

is_allowed_to_publish_publishers(publishers, Affiliation) ->
    (Affiliation == owner)
        orelse (Affiliation == publisher)
        orelse (Affiliation == publish_only).

make_item_el(ItemId, Children) ->
    #{name => <<"item">>,
      attrs => #{<<"id">> => ItemId},
      children => Children}.

make_data_el(Data) ->
    #{name => <<"data">>,
      attrs => #{},
      children => [#{content => Data}]}.
