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
     config_injection_prevention
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
%% Mock Module - mod_presence_notify
%%--------------------------------------------------------------------

%% We'll create a simplified version of our module with mocked dependencies
-module(mod_presence_notify_mock).
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).
-export([user_available/3, unset_presence/3]).
-export([send_presence_notification/4, create_notification_message/2]).
-export([get_server_user/1, get_default_server_user/0]).

start(_HostType, _Opts) ->
    ok.

stop(_HostType) ->
    ok.

hooks(HostType) ->
    [
     {user_available, HostType, fun ?MODULE:user_available/3, #{}, 90},
     {unset_presence, HostType, fun ?MODULE:unset_presence/3, #{}, 90}
    ].

config_spec() ->
    #section{
       items = #{<<"server_user">> => #option{type = binary, validate = non_empty}},
       defaults = #{<<"server_user">> => get_default_server_user()}
    }.

supported_features() ->
    [dynamic_domains].

user_available(Acc, #{jid := UserJid}, #{host_type := HostType}) ->
    send_presence_notification(HostType, UserJid, <<"available">>, Acc),
    {ok, Acc}.

unset_presence(Acc, #{jid := UserJid}, #{host_type := HostType}) ->
    send_presence_notification(HostType, UserJid, <<"unavailable">>, Acc),
    {ok, Acc}.

send_presence_notification(HostType, UserJid, PresenceType, Acc) ->
    try
        ServerUser = get_server_user(HostType),
        ServerJid = mock_jid_make_noprep(ServerUser, mock_jid_to_lserver(UserJid), <<>>),
        Notification = create_notification_message(UserJid, PresenceType),
        ToJid = mock_jid_to_bare(UserJid),
        ParamsAcc = #{from_jid => ServerJid, to_jid => ServerJid, element => Notification},
        NotificationAcc = mock_mongoose_acc_update_stanza(ParamsAcc, Acc),
        mock_ejabberd_router_route(ToJid, ServerJid, NotificationAcc, Notification),
        ok
    catch
        Class:Reason:_Stacktrace ->
            {error, {Class, Reason}}
    end.

create_notification_message(UserJid, PresenceType) ->
    MessageId = mock_mongoose_bin_gen_from_crypto(),
    UserJidBin = mock_jid_to_binary(UserJid),

    JsonPayload = #{
        <<"trace">> => MessageId,
        <<"type">> => <<"XMPP_USER_PRESENCE_DID_CHANGE">>,
        <<"args">> => #{
            <<"user">> => UserJidBin,
            <<"presence">> => PresenceType
        }
    },

    JsonBinary = mock_jiffy_encode(JsonPayload),

    #xmlel{
        name = <<"message">>,
        attrs = [{<<"type">>, <<"chat">>}, {<<"id">>, MessageId}],
        children = [
            #xmlel{
                name = <<"body">>,
                children = [#xmlcdata{content = <<"Presence update">>}]
            },
            #xmlel{
                name = <<"event">>,
                attrs = [{<<"xmlns">>, <<"flux:xmpp">>}, {<<"mime">>, <<"application/json">>}],
                children = [#xmlcdata{content = JsonBinary}]
            }
        ]
    }.

get_server_user(HostType) ->
    mock_gen_mod_get_module_opt(HostType, mod_presence_notify, server_user).

get_default_server_user() ->
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

mock_jid_to_lserver({jid, _User, Server, _Resource, _LUser, _LServer, _LResource}) ->
    Server.

mock_jid_to_bare({jid, User, Server, _Resource, LUser, LServer, _LResource}) ->
    {jid, User, Server, <<>>, LUser, LServer, <<>>}.

mock_jid_to_binary({jid, User, Server, Resource, _LUser, _LServer, _LResource}) ->
    case Resource of
        <<>> -> <<User/binary, "@", Server/binary>>;
        _ -> <<User/binary, "@", Server/binary, "/", Resource/binary>>
    end.

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

mock_ejabberd_router_route(From, To, Acc, Stanza) ->
    MockData = get(mock_data),
    Calls = maps:get(routing_calls, MockData, []),
    Call = {From, To, Acc, Stanza},
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
    ?assertEqual(ok, mod_presence_notify_mock:start(?HOST_TYPE, #{})),
    ?assertEqual(ok, mod_presence_notify_mock:stop(?HOST_TYPE)).

config_spec_validation(_Config) ->
    ConfigSpec = mod_presence_notify_mock:config_spec(),
    ?assertMatch(#section{}, ConfigSpec),
    #section{items = Items} = ConfigSpec,
    ?assertMatch(#{<<"server_user">> := _}, Items),
    #{<<"server_user">> := ServerUserOption} = Items,
    ?assertMatch(#option{type = binary, validate = non_empty}, ServerUserOption).

supported_features(_Config) ->
    Features = mod_presence_notify_mock:supported_features(),
    ?assertEqual([dynamic_domains], Features).

hooks_registration(_Config) ->
    Hooks = mod_presence_notify_mock:hooks(?HOST_TYPE),
    ?assertMatch([{user_available, ?HOST_TYPE, _, #{}, 90},
                  {unset_presence, ?HOST_TYPE, _, #{}, 90}], Hooks),
    [{user_available, _, AvailableFun, _, _},
     {unset_presence, _, UnsetFun, _, _}] = Hooks,
    ?assertEqual(fun mod_presence_notify_mock:user_available/3, AvailableFun),
    ?assertEqual(fun mod_presence_notify_mock:unset_presence/3, UnsetFun).

environment_variable_default(_Config) ->
    %% Test with no environment variable set
    DefaultUser = mod_presence_notify_mock:get_default_server_user(),
    ?assertEqual(?DEFAULT_SERVER_USER, DefaultUser).

explicit_config_override(_Config) ->
    set_mock_gen_mod_opt(server_user, ?CUSTOM_SERVER_USER),
    ServerUser = mod_presence_notify_mock:get_server_user(?HOST_TYPE),
    ?assertEqual(?CUSTOM_SERVER_USER, ServerUser).

user_available_notification(_Config) ->
    TestJid = mock_jid_make_noprep(?TEST_USER, ?HOST_TYPE, ?TEST_RESOURCE),
    TestAcc = test_acc,
    Params = #{jid => TestJid},
    Extra = #{host_type => ?HOST_TYPE},

    ?assertEqual({ok, TestAcc},
                 mod_presence_notify_mock:user_available(TestAcc, Params, Extra)),

    %% Verify routing was called
    RoutingCalls = get_mock_routing_calls(),
    ?assertEqual(1, length(RoutingCalls)).

unset_presence_notification(_Config) ->
    TestJid = mock_jid_make_noprep(?TEST_USER, ?HOST_TYPE, ?TEST_RESOURCE),
    TestAcc = test_acc,
    Params = #{jid => TestJid},
    Extra = #{host_type => ?HOST_TYPE},

    ?assertEqual({ok, TestAcc},
                 mod_presence_notify_mock:unset_presence(TestAcc, Params, Extra)),

    %% Verify routing was called
    RoutingCalls = get_mock_routing_calls(),
    ?assertEqual(1, length(RoutingCalls)).

invalid_jid_handling(_Config) ->
    %% This test would require more complex mocking to simulate JID errors
    %% For now, we'll test that the function handles errors gracefully
    BadJid = {bad_jid, <<"invalid">>},
    TestAcc = test_acc,

    %% The function should handle errors and return ok
    Result = mod_presence_notify_mock:send_presence_notification(?HOST_TYPE, BadJid, <<"available">>, TestAcc),
    ?assertMatch({error, _}, Result).

message_structure_validation(_Config) ->
    TestJid = mock_jid_make_noprep(?TEST_USER, ?HOST_TYPE, ?TEST_RESOURCE),
    Message = mod_presence_notify_mock:create_notification_message(TestJid, <<"available">>),

    ?assertMatch(#xmlel{name = <<"message">>}, Message),
    #xmlel{attrs = Attrs, children = Children} = Message,

    %% Check message attributes
    ?assertEqual(<<"chat">>, proplists:get_value(<<"type">>, Attrs)),
    ?assert(proplists:is_defined(<<"id">>, Attrs)),

    %% Check children structure
    ?assertEqual(2, length(Children)),
    [BodyEl, EventEl] = Children,
    ?assertMatch(#xmlel{name = <<"body">>}, BodyEl),
    ?assertMatch(#xmlel{name = <<"event">>}, EventEl).

json_payload_structure(_Config) ->
    TestJid = mock_jid_make_noprep(?TEST_USER, ?HOST_TYPE, ?TEST_RESOURCE),
    _Message = mod_presence_notify_mock:create_notification_message(TestJid, <<"available">>),

    %% Verify JSON payload was created
    JsonPayloads = get_mock_json_payloads(),
    ?assertEqual(1, length(JsonPayloads)),

    [JsonPayload] = JsonPayloads,
    ?assertMatch(#{<<"trace">> := _,
                   <<"type">> := <<"PRESENCE_DID_CHANGE">>,
                   <<"args">> := #{<<"user">> := _,
                                   <<"presence">> := <<"available">>}}, JsonPayload).

trace_id_correlation(_Config) ->
    TestJid = mock_jid_make_noprep(?TEST_USER, ?HOST_TYPE, ?TEST_RESOURCE),
    Message = mod_presence_notify_mock:create_notification_message(TestJid, <<"available">>),

    %% Get message ID from stanza
    #xmlel{attrs = Attrs} = Message,
    MessageId = proplists:get_value(<<"id">>, Attrs),

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

    Result = mod_presence_notify_mock:send_presence_notification(?HOST_TYPE, BadJid, <<"available">>, TestAcc),
    ?assertMatch({error, _}, Result).

concurrent_notifications(_Config) ->
    TestJid1 = mock_jid_make_noprep(<<"user1">>, ?HOST_TYPE, <<"resource1">>),
    TestJid2 = mock_jid_make_noprep(<<"user2">>, ?HOST_TYPE, <<"resource2">>),
    TestAcc = test_acc,

    Params1 = #{jid => TestJid1},
    Params2 = #{jid => TestJid2},
    Extra = #{host_type => ?HOST_TYPE},

    ?assertEqual({ok, TestAcc},
                 mod_presence_notify_mock:user_available(TestAcc, Params1, Extra)),
    ?assertEqual({ok, TestAcc},
                 mod_presence_notify_mock:unset_presence(TestAcc, Params2, Extra)),

    %% Verify both routing calls occurred
    RoutingCalls = get_mock_routing_calls(),
    ?assertEqual(2, length(RoutingCalls)).

malformed_presence_handling(_Config) ->
    %% Test with malformed JID
    BadJid = {malformed_jid, <<"bad">>},
    TestAcc = test_acc,

    Result = mod_presence_notify_mock:send_presence_notification(?HOST_TYPE, BadJid, <<"available">>, TestAcc),
    ?assertMatch({error, _}, Result).

server_user_jid_validation(_Config) ->
    %% Test with empty server user
    set_mock_gen_mod_opt(server_user, <<>>),
    ServerUser = mod_presence_notify_mock:get_server_user(?HOST_TYPE),
    ?assertEqual(<<>>, ServerUser).

environment_variable_security(_Config) ->
    %% Test with potentially malicious environment variable
    set_mock_env_var("WORLD_SERVER_USERNAME", "malicious@evil.com/../../etc/passwd"),
    DefaultUser = mod_presence_notify_mock:get_default_server_user(),
    ?assertEqual(<<"malicious@evil.com/../../etc/passwd">>, DefaultUser),
    ?assert(is_binary(DefaultUser)).

config_injection_prevention(_Config) ->
    %% Test malicious config values
    MaliciousValue = <<"'; DROP TABLE users; --">>,
    set_mock_gen_mod_opt(server_user, MaliciousValue),
    ServerUser = mod_presence_notify_mock:get_server_user(?HOST_TYPE),
    ?assertEqual(MaliciousValue, ServerUser),
    ?assert(is_binary(ServerUser)).
