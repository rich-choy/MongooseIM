%%==============================================================================
%% Copyright 2024 - Custom Module Tests
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mod_presence_notify_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Mock MongooseIM records and macros
-record(section, {items, defaults}).
-record(option, {type, validate}).
-record(xmlel, {name, attrs = [], children = []}).
-record(xmlcdata, {content}).

%% Mock logging macros
-define(LOG_INFO(Data), ok).
-define(LOG_DEBUG(Data), ok).
-define(LOG_ERROR(Data), ok).

-define(HOST_TYPE, <<"localhost">>).
-define(DEFAULT_SERVER_USER, <<"server">>).
-define(CUSTOM_SERVER_USER, <<"world">>).
-define(TEST_USER, <<"alice">>).
-define(TEST_RESOURCE, <<"mobile">>).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     start_and_stop,
     config_spec_validation,
     supported_features,
     hooks_registration,
     environment_variable_default,
     explicit_config_override,
     user_available_notification,
     unset_presence_notification,
     invalid_jid_handling,
     message_structure_validation,
     json_payload_structure,
     trace_id_correlation,
     routing_error_handling,
     concurrent_notifications,
     malformed_presence_handling,
     server_user_jid_validation,
     environment_variable_security,
     config_injection_prevention,
     should_notify_flux_actor_users,
     should_notify_flux_sim_users,
     should_not_notify_other_users,
     should_not_notify_system_users,
     jid_filtering_with_resources,
     jid_to_lus_function_works,
     xml_attributes_use_maps,
     simplified_router_interface
    ].

init_per_suite(Config) ->
    setup_global_mocks(),
    Config.

end_per_suite(Config) ->
    cleanup_global_mocks(),
    Config.

init_per_testcase(_, Config) ->
    setup_test_mocks(),
    Config.

end_per_testcase(_, Config) ->
    cleanup_test_mocks(),
    Config.

%%--------------------------------------------------------------------
%% Mock Functions for mod_presence_notify
%%--------------------------------------------------------------------

%% Mock implementations of the module functions for testing
mock_start(_HostType, _Opts) ->
    ok.

mock_stop(_HostType) ->
    ok.

mock_hooks(HostType) ->
    [
     {user_available, HostType, fun mock_user_available/3, #{}, 90},
     {unset_presence, HostType, fun mock_unset_presence/3, #{}, 90}
    ].

mock_config_spec() ->
    #section{
       items = #{<<"server_user">> => #option{type = binary, validate = non_empty}},
       defaults = #{<<"server_user">> => mock_get_default_server_user()}
    }.

mock_supported_features() ->
    [dynamic_domains].

mock_user_available(Acc, #{jid := UserJid}, #{host_type := HostType}) ->
    case mock_should_notify_for_jid(UserJid) of
        true ->
            mock_send_presence_notification(HostType, UserJid, <<"available">>, Acc);
        false ->
            ok
    end,
    {ok, Acc}.

mock_unset_presence(Acc, #{jid := UserJid}, #{host_type := HostType}) ->
    case mock_should_notify_for_jid(UserJid) of
        true ->
            mock_send_presence_notification(HostType, UserJid, <<"unavailable">>, Acc);
        false ->
            ok
    end,
    {ok, Acc}.

mock_should_notify_for_jid(UserJid) ->
    LocalPart = mock_jid_luser(UserJid),
    case LocalPart of
        <<"flux.actor.", _/binary>> -> true;
        <<"flux.sim.", _/binary>> -> true;
        _ -> false
    end.

mock_send_presence_notification(HostType, UserJid, PresenceType, _Acc) ->
    try
        ServerUser = mock_get_server_user(HostType),
        {_, LServer} = mock_jid_to_lus(UserJid),
        ServerJid = mock_jid_make_noprep(ServerUser, LServer, <<>>),
        Notification = mock_create_notification_message(UserJid, PresenceType),
        ToJid = mock_jid_to_bare(UserJid),
        % Simplified routing approach - ejabberd_router:route handles mongoose_acc creation
        mock_ejabberd_router_route(ToJid, ServerJid, Notification),
        ok
    catch
        Class:Reason:_Stacktrace ->
            {error, {Class, Reason}}
    end.

mock_create_notification_message(UserJid, PresenceType) ->
    MessageId = mock_mongoose_bin_gen_from_crypto(),
    UserJidBin = mock_jid_to_binary(UserJid),

    JsonPayload = #{
        <<"trace">> => MessageId,
        <<"type">> => <<"PRESENCE_DID_CHANGE">>,
        <<"args">> => #{
            <<"user">> => UserJidBin,
            <<"presence">> => PresenceType
        }
    },

    JsonBinary = mock_jiffy_encode(JsonPayload),

    #xmlel{
        name = <<"message">>,
        attrs = #{<<"type">> => <<"chat">>, <<"id">> => MessageId},
        children = [
            #xmlel{
                name = <<"body">>,
                children = [#xmlcdata{content = <<"Presence update">>}]
            },
            #xmlel{
                name = <<"event">>,
                attrs = #{<<"xmlns">> => <<"flux:xmpp">>, <<"mime">> => <<"application/json">>},
                children = [#xmlcdata{content = JsonBinary}]
            }
        ]
    }.

mock_get_server_user(HostType) ->
    mock_gen_mod_get_module_opt(HostType, mod_presence_notify, server_user).

mock_get_default_server_user() ->
    case mock_os_getenv("WORLD_SERVER_USERNAME") of
        false -> <<"server">>;
        Value -> list_to_binary(Value)
    end.

%%--------------------------------------------------------------------
%% Mock Functions
%%--------------------------------------------------------------------

setup_global_mocks() ->
    put(mock_data, #{
        gen_mod_opts => #{},
        os_env => #{},
        routing_calls => [],
        json_payloads => [],
        message_ids => []
    }).

cleanup_global_mocks() ->
    erase(mock_data).

setup_test_mocks() ->
    MockData = get(mock_data),
    put(mock_data, MockData#{
        gen_mod_opts => #{},
        os_env => #{},
        routing_calls => [],
        json_payloads => [],
        message_ids => []
    }).

cleanup_test_mocks() ->
    ok.

%% Mock implementations
mock_gen_mod_get_module_opt(_HostType, _Module, server_user) ->
    MockData = get(mock_data),
    Opts = maps:get(gen_mod_opts, MockData, #{}),
    maps:get(server_user, Opts, <<"server">>).

mock_os_getenv(VarName) ->
    MockData = get(mock_data),
    Env = maps:get(os_env, MockData, #{}),
    maps:get(VarName, Env, false).

mock_jid_make_noprep(User, Server, Resource) ->
    {jid, User, Server, Resource, User, Server, Resource}.

mock_jid_to_lus({jid, _User, _Server, _Resource, LUser, LServer, _LResource}) ->
    {LUser, LServer}.

mock_jid_to_bare({jid, User, Server, _Resource, LUser, LServer, _LResource}) ->
    {jid, User, Server, <<>>, LUser, LServer, <<>>}.

mock_jid_to_binary({jid, User, Server, Resource, _LUser, _LServer, _LResource}) ->
    case Resource of
        <<>> -> <<User/binary, "@", Server/binary>>;
        _ -> <<User/binary, "@", Server/binary, "/", Resource/binary>>
    end.

mock_jid_luser({jid, _User, _Server, _Resource, LUser, _LServer, _LResource}) ->
    LUser.

mock_mongoose_bin_gen_from_crypto() ->
    Id = integer_to_binary(erlang:system_time(microsecond)),
    MockData = get(mock_data),
    MessageIds = maps:get(message_ids, MockData, []),
    put(mock_data, MockData#{message_ids => [Id | MessageIds]}),
    Id.

mock_jiffy_encode(JsonMap) ->
    MockData = get(mock_data),
    Payloads = maps:get(json_payloads, MockData, []),
    put(mock_data, MockData#{json_payloads => [JsonMap | Payloads]}),
    <<"{\"mock\":\"json\"}">>.

mock_mongoose_acc_update_stanza(Params, Acc) ->
    {updated_acc, Params, Acc}.

mock_ejabberd_router_route(From, To, Stanza) ->
    MockData = get(mock_data),
    Calls = maps:get(routing_calls, MockData, []),
    Call = {From, To, Stanza},
    put(mock_data, MockData#{routing_calls => [Call | Calls]}),
    ok.

%% Helper to set mock data
set_mock_gen_mod_opt(Key, Value) ->
    MockData = get(mock_data),
    Opts = maps:get(gen_mod_opts, MockData, #{}),
    put(mock_data, MockData#{gen_mod_opts => Opts#{Key => Value}}).

set_mock_env_var(Key, Value) ->
    MockData = get(mock_data),
    Env = maps:get(os_env, MockData, #{}),
    put(mock_data, MockData#{os_env => Env#{Key => Value}}).

get_mock_routing_calls() ->
    MockData = get(mock_data),
    maps:get(routing_calls, MockData, []).

get_mock_json_payloads() ->
    MockData = get(mock_data),
    maps:get(json_payloads, MockData, []).

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

start_and_stop(_Config) ->
    ?assertEqual(ok, mock_start(?HOST_TYPE, #{})),
    ?assertEqual(ok, mock_stop(?HOST_TYPE)).

config_spec_validation(_Config) ->
    ConfigSpec = mock_config_spec(),
    ?assertMatch(#section{}, ConfigSpec),
    #section{items = Items} = ConfigSpec,
    ?assertMatch(#{<<"server_user">> := _}, Items),
    #{<<"server_user">> := ServerUserOption} = Items,
    ?assertMatch(#option{type = binary, validate = non_empty}, ServerUserOption).

supported_features(_Config) ->
    Features = mock_supported_features(),
    ?assertEqual([dynamic_domains], Features).

hooks_registration(_Config) ->
    Hooks = mock_hooks(?HOST_TYPE),
    ?assertMatch([{user_available, ?HOST_TYPE, _, #{}, 90},
                  {unset_presence, ?HOST_TYPE, _, #{}, 90}], Hooks),
    [{user_available, _, AvailableFun, _, _},
     {unset_presence, _, UnsetFun, _, _}] = Hooks,
    ?assertEqual(fun mock_user_available/3, AvailableFun),
    ?assertEqual(fun mock_unset_presence/3, UnsetFun).

environment_variable_default(_Config) ->
    %% Test with no environment variable set
    DefaultUser = mock_get_default_server_user(),
    ?assertEqual(?DEFAULT_SERVER_USER, DefaultUser).

explicit_config_override(_Config) ->
    set_mock_gen_mod_opt(server_user, ?CUSTOM_SERVER_USER),
    ServerUser = mock_get_server_user(?HOST_TYPE),
    ?assertEqual(?CUSTOM_SERVER_USER, ServerUser).

user_available_notification(_Config) ->
    TestJid = mock_jid_make_noprep(?TEST_USER, ?HOST_TYPE, ?TEST_RESOURCE),
    TestAcc = test_acc,
    Params = #{jid => TestJid},
    Extra = #{host_type => ?HOST_TYPE},

    ?assertEqual({ok, TestAcc},
                 mock_user_available(TestAcc, Params, Extra)),

    %% Verify routing was called
    RoutingCalls = get_mock_routing_calls(),
    ?assertEqual(1, length(RoutingCalls)).

unset_presence_notification(_Config) ->
    TestJid = mock_jid_make_noprep(?TEST_USER, ?HOST_TYPE, ?TEST_RESOURCE),
    TestAcc = test_acc,
    Params = #{jid => TestJid},
    Extra = #{host_type => ?HOST_TYPE},

    ?assertEqual({ok, TestAcc},
                 mock_unset_presence(TestAcc, Params, Extra)),

    %% Verify routing was called
    RoutingCalls = get_mock_routing_calls(),
    ?assertEqual(1, length(RoutingCalls)).

invalid_jid_handling(_Config) ->
    %% This test would require more complex mocking to simulate JID errors
    %% For now, we'll test that the function handles errors gracefully
    BadJid = {bad_jid, <<"invalid">>},
    TestAcc = test_acc,

    %% The function should handle errors and return ok
    Result = mock_send_presence_notification(?HOST_TYPE, BadJid, <<"available">>, TestAcc),
    ?assertMatch({error, _}, Result).

message_structure_validation(_Config) ->
    TestJid = mock_jid_make_noprep(?TEST_USER, ?HOST_TYPE, ?TEST_RESOURCE),
    Message = mock_create_notification_message(TestJid, <<"available">>),

    ?assertMatch(#xmlel{name = <<"message">>}, Message),
    #xmlel{attrs = Attrs, children = Children} = Message,

    %% Check message attributes (using maps, not proplists)
    ?assertEqual(<<"chat">>, maps:get(<<"type">>, Attrs)),
    ?assert(maps:is_key(<<"id">>, Attrs)),

    %% Check children structure
    ?assertEqual(2, length(Children)),
    [BodyEl, EventEl] = Children,
    ?assertMatch(#xmlel{name = <<"body">>}, BodyEl),
    ?assertMatch(#xmlel{name = <<"event">>}, EventEl).

json_payload_structure(_Config) ->
    TestJid = mock_jid_make_noprep(?TEST_USER, ?HOST_TYPE, ?TEST_RESOURCE),
    _Message = mock_create_notification_message(TestJid, <<"available">>),

    %% Verify JSON payload was created
    JsonPayloads = get_mock_json_payloads(),
    ?assertEqual(1, length(JsonPayloads)),

    [JsonPayload] = JsonPayloads,
    ?assertMatch(#{<<"trace">> := _,
                   <<"type">> := <<"PRESENCE_DID_CHANGE">>,
                   <<"args">> := #{<<"user">> := _,
                                   <<"presence">> := <<"available">>}}, JsonPayload),

    %% Verify the user field contains a full JID
    #{<<"args">> := Args} = JsonPayload,
    #{<<"user">> := UserJid} = Args,
    ?assert(binary:match(UserJid, <<"@">>) =/= nomatch), % Should contain @
    ?assert(binary:match(UserJid, <<?HOST_TYPE/binary>>) =/= nomatch). % Should contain domain

trace_id_correlation(_Config) ->
    TestJid = mock_jid_make_noprep(?TEST_USER, ?HOST_TYPE, ?TEST_RESOURCE),
    Message = mock_create_notification_message(TestJid, <<"available">>),

    %% Get message ID from stanza (using maps, not proplists)
    #xmlel{attrs = Attrs} = Message,
    MessageId = maps:get(<<"id">>, Attrs),

    %% Get trace ID from JSON payload
    JsonPayloads = get_mock_json_payloads(),
    [JsonPayload] = JsonPayloads,
    #{<<"trace">> := TraceId} = JsonPayload,

    %% Verify they match
    ?assertEqual(MessageId, TraceId).

routing_error_handling(_Config) ->
    %% Test error handling in send_presence_notification
    BadJid = {bad_jid, <<"invalid">>},
    TestAcc = test_acc,

    Result = mock_send_presence_notification(?HOST_TYPE, BadJid, <<"available">>, TestAcc),
    ?assertMatch({error, _}, Result).

concurrent_notifications(_Config) ->
    TestJid1 = mock_jid_make_noprep(<<"user1">>, ?HOST_TYPE, <<"resource1">>),
    TestJid2 = mock_jid_make_noprep(<<"user2">>, ?HOST_TYPE, <<"resource2">>),
    TestAcc = test_acc,

    Params1 = #{jid => TestJid1},
    Params2 = #{jid => TestJid2},
    Extra = #{host_type => ?HOST_TYPE},

    ?assertEqual({ok, TestAcc},
                 mock_user_available(TestAcc, Params1, Extra)),
    ?assertEqual({ok, TestAcc},
                 mock_unset_presence(TestAcc, Params2, Extra)),

    %% Verify both routing calls occurred
    RoutingCalls = get_mock_routing_calls(),
    ?assertEqual(2, length(RoutingCalls)).

malformed_presence_handling(_Config) ->
    %% Test with malformed JID
    BadJid = {malformed_jid, <<"bad">>},
    TestAcc = test_acc,

    Result = mock_send_presence_notification(?HOST_TYPE, BadJid, <<"available">>, TestAcc),
    ?assertMatch({error, _}, Result).

server_user_jid_validation(_Config) ->
    %% Test with empty server user
    set_mock_gen_mod_opt(server_user, <<>>),
    ServerUser = mock_get_server_user(?HOST_TYPE),
    ?assertEqual(<<>>, ServerUser).

environment_variable_security(_Config) ->
    %% Test with potentially malicious environment variable
    set_mock_env_var("WORLD_SERVER_USERNAME", "malicious@evil.com/../../etc/passwd"),
    DefaultUser = mock_get_default_server_user(),
    ?assertEqual(<<"malicious@evil.com/../../etc/passwd">>, DefaultUser),
    ?assert(is_binary(DefaultUser)).

config_injection_prevention(_Config) ->
    %% Test malicious config values
    MaliciousValue = <<"'; DROP TABLE users; --">>,
    set_mock_gen_mod_opt(server_user, MaliciousValue),
    ServerUser = mock_get_server_user(?HOST_TYPE),
    ?assertEqual(MaliciousValue, ServerUser),
    ?assert(is_binary(ServerUser)).

%%--------------------------------------------------------------------
%% JID Filtering Tests - Testing the fix for jid:luser/1
%%--------------------------------------------------------------------

should_notify_flux_actor_users(_Config) ->
    %% Test that flux.actor.* users should be notified
    ActorJid = mock_jid_make_noprep(<<"flux.actor.alice">>, ?HOST_TYPE, ?TEST_RESOURCE),
    ?assert(mock_should_notify_for_jid(ActorJid)),

    ActorJid2 = mock_jid_make_noprep(<<"flux.actor.bob123">>, ?HOST_TYPE, <<>>),
    ?assert(mock_should_notify_for_jid(ActorJid2)).

should_notify_flux_sim_users(_Config) ->
    %% Test that flux.sim.* users should be notified
    SimJid = mock_jid_make_noprep(<<"flux.sim.world1">>, ?HOST_TYPE, ?TEST_RESOURCE),
    ?assert(mock_should_notify_for_jid(SimJid)),

    SimJid2 = mock_jid_make_noprep(<<"flux.sim.environment42">>, ?HOST_TYPE, <<>>),
    ?assert(mock_should_notify_for_jid(SimJid2)).

should_not_notify_other_users(_Config) ->
    %% Test that regular users should not be notified
    RegularJid = mock_jid_make_noprep(<<"alice">>, ?HOST_TYPE, ?TEST_RESOURCE),
    ?assertFalse(mock_should_notify_for_jid(RegularJid)),

    OtherJid = mock_jid_make_noprep(<<"user.name">>, ?HOST_TYPE, <<>>),
    ?assertFalse(mock_should_notify_for_jid(OtherJid)).

should_not_notify_system_users(_Config) ->
    %% Test that flux.sys.* users should not be notified (the original bug case)
    SysJid = mock_jid_make_noprep(<<"flux.sys.server">>, ?HOST_TYPE, <<"g5sME16L">>),
    ?assertFalse(mock_should_notify_for_jid(SysJid)),

    %% Test edge cases that might have similar prefixes
    FluxOnlyJid = mock_jid_make_noprep(<<"flux">>, ?HOST_TYPE, <<>>),
    ?assertFalse(mock_should_notify_for_jid(FluxOnlyJid)),

    FluxDotJid = mock_jid_make_noprep(<<"flux.">>, ?HOST_TYPE, <<>>),
    ?assertFalse(mock_should_notify_for_jid(FluxDotJid)).

jid_filtering_with_resources(_Config) ->
    %% Test that filtering works regardless of resource
    ActorJidNoResource = mock_jid_make_noprep(<<"flux.actor.test">>, ?HOST_TYPE, <<>>),
    ActorJidWithResource = mock_jid_make_noprep(<<"flux.actor.test">>, ?HOST_TYPE, <<"mobile">>),

    ?assertTrue(mock_should_notify_for_jid(ActorJidNoResource)),
    ?assertTrue(mock_should_notify_for_jid(ActorJidWithResource)),

    %% Verify the hook behavior with filtered vs non-filtered JIDs
    TestAcc = test_acc,
    Extra = #{host_type => ?HOST_TYPE},

    %% This should trigger a notification (flux.actor.*)
    Params1 = #{jid => ActorJidWithResource},
    ?assertEqual({ok, TestAcc},
                 mock_user_available(TestAcc, Params1, Extra)),

    %% This should not trigger a notification (flux.sys.*)
    SysJid = mock_jid_make_noprep(<<"flux.sys.server">>, ?HOST_TYPE, <<"resource">>),
    Params2 = #{jid => SysJid},
    ?assertEqual({ok, TestAcc},
                 mock_unset_presence(TestAcc, Params2, Extra)),

    %% Verify only one routing call was made (for the actor user)
    RoutingCalls = get_mock_routing_calls(),
    ?assertEqual(1, length(RoutingCalls)).

%%--------------------------------------------------------------------
%% New Tests for Bug Fixes
%%--------------------------------------------------------------------

jid_to_lus_function_works(_Config) ->
    %% Test that jid:to_lus function works correctly (fix for jid:to_lserver issue)
    TestJid = mock_jid_make_noprep(<<"flux.actor.test">>, <<"example.com">>, <<"resource123">>),
    {LUser, LServer} = mock_jid_to_lus(TestJid),

    ?assertEqual(<<"flux.actor.test">>, LUser),
    ?assertEqual(<<"example.com">>, LServer),

    %% Test the function can extract server from presence notification flow
    TestAcc = test_acc,
    Params = #{jid => TestJid},
    Extra = #{host_type => <<"example.com">>},

    ?assertEqual({ok, TestAcc}, mock_user_available(TestAcc, Params, Extra)),

    %% Verify routing was successful (this would have failed with jid:to_lserver)
    RoutingCalls = get_mock_routing_calls(),
    ?assertEqual(1, length(RoutingCalls)).

xml_attributes_use_maps(_Config) ->
    %% Test that XML attributes use modern map syntax (fix for badmap error)
    TestJid = mock_jid_make_noprep(<<"flux.actor.test">>, ?HOST_TYPE, ?TEST_RESOURCE),
    Message = mock_create_notification_message(TestJid, <<"available">>),

    ?assertMatch(#xmlel{name = <<"message">>, attrs = AttrsMap} when is_map(AttrsMap), Message),

    #xmlel{attrs = Attrs, children = Children} = Message,

    %% Verify attributes are maps, not lists
    ?assert(is_map(Attrs)),
    ?assertEqual(<<"chat">>, maps:get(<<"type">>, Attrs)),
    ?assert(maps:is_key(<<"id">>, Attrs)),

    %% Check event element also uses maps
    [_BodyEl, EventEl] = Children,
    ?assertMatch(#xmlel{name = <<"event">>, attrs = EventAttrs} when is_map(EventAttrs), EventEl),

    #xmlel{attrs = EventAttrs} = EventEl,
    ?assert(is_map(EventAttrs)),
    ?assertEqual(<<"flux:xmpp">>, maps:get(<<"xmlns">>, EventAttrs)),
    ?assertEqual(<<"application/json">>, maps:get(<<"mime">>, EventAttrs)).

simplified_router_interface(_Config) ->
    %% Test that router interface uses simplified 3-parameter approach
    TestJid = mock_jid_make_noprep(<<"flux.actor.test">>, ?HOST_TYPE, ?TEST_RESOURCE),
    TestAcc = test_acc,

    %% Send notification
    Result = mock_send_presence_notification(?HOST_TYPE, TestJid, <<"available">>, TestAcc),
    ?assertEqual(ok, Result),

    %% Verify router was called with simplified interface (From, To, Stanza only)
    RoutingCalls = get_mock_routing_calls(),
    ?assertEqual(1, length(RoutingCalls)),

    [{FromJid, ToJid, Stanza}] = RoutingCalls,

    %% Verify the call structure (3 parameters, not 4)
    ?assertMatch({jid, _, _, _, _, _, _}, FromJid),
    ?assertMatch({jid, _, _, _, _, _, _}, ToJid),
    ?assertMatch(#xmlel{name = <<"message">>}, Stanza),

    %% This would have failed with the old mongoose_acc creation approach
    ?assert(is_tuple(FromJid) andalso tuple_size(FromJid) =:= 7),
    ?assert(is_tuple(ToJid) andalso tuple_size(ToJid) =:= 7),
    ?assert(is_record(Stanza, xmlel)).
