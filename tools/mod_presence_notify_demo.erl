#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

-mode(compile).

%% Mock records for demonstration
-record(xmlel, {name, attrs = [], children = []}).
-record(xmlcdata, {content}).

main([]) ->
    io:format("=== MongooseIM mod_presence_notify Integration Demo ===~n~n"),

    %% Simulate MongooseIM startup
    simulate_mongooseim_startup(),

    %% Simulate presence changes
    simulate_presence_changes(),

    %% Show configuration examples
    show_configuration_examples(),

    io:format("~n=== Demo completed successfully! ===~n").

simulate_mongooseim_startup() ->
    io:format("1. MongooseIM Server Startup Simulation~n"),
    io:format("   =====================================~n"),

    %% Simulate module loading
    HostType = <<"localhost">>,
    ModuleOpts = #{},

    io:format("   • Loading mod_presence_notify for host: ~s~n", [HostType]),

    %% Simulate start/2 callback
    case start_simulation(HostType, ModuleOpts) of
        ok ->
            io:format("   ✓ Module started successfully~n");
        Error ->
            io:format("   ✗ Module start failed: ~p~n", [Error])
    end,

    %% Show hooks registration
    Hooks = hooks_simulation(HostType),
    io:format("   • Registered hooks:~n"),
    lists:foreach(fun({HookName, _, _, _, Priority}) ->
        io:format("     - ~s (priority: ~p)~n", [HookName, Priority])
    end, Hooks),

    %% Show configuration
    ConfigSpec = config_spec_simulation(),
    io:format("   • Configuration schema loaded~n"),
    io:format("   • Supported features: ~p~n", [supported_features_simulation()]),

    io:format("~n").

simulate_presence_changes() ->
    io:format("2. Presence Change Simulation~n"),
    io:format("   ===========================~n"),

    %% Set up environment
    os:putenv("WORLD_SERVER_USERNAME", "world_server"),

    HostType = <<"localhost">>,

    %% Simulate different users coming online/offline
    Users = [
        {<<"alice">>, <<"localhost">>, <<"mobile">>},
        {<<"bob">>, <<"localhost">>, <<"desktop">>},
        {<<"charlie">>, <<"localhost">>, <<"web">>}
    ],

    %% Simulate user_available events
    io:format("   Simulating users coming online:~n"),
    lists:foreach(fun({User, Server, Resource}) ->
        UserJid = {jid, User, Server, Resource, User, Server, Resource},
        simulate_user_available(HostType, UserJid)
    end, Users),

    io:format("~n   Simulating users going offline:~n"),
    lists:foreach(fun({User, Server, Resource}) ->
        UserJid = {jid, User, Server, Resource, User, Server, Resource},
        simulate_unset_presence(HostType, UserJid)
    end, Users),

    io:format("~n").

simulate_user_available(HostType, UserJid) ->
    UserJidBin = jid_to_binary_mock(UserJid),
    io:format("   • ~s comes online~n", [UserJidBin]),

    %% Simulate the hook call
    Acc = mock_acc,
    Params = #{jid => UserJid},
    Extra = #{host_type => HostType},

    %% This would trigger our user_available/3 function
    Result = user_available_simulation(Acc, Params, Extra),

    case Result of
        {ok, _} ->
            io:format("     → Notification sent to world_server@localhost~n");
        Error ->
            io:format("     → Error: ~p~n", [Error])
    end.

simulate_unset_presence(HostType, UserJid) ->
    UserJidBin = jid_to_binary_mock(UserJid),
    io:format("   • ~s goes offline~n", [UserJidBin]),

    %% Simulate the hook call
    Acc = mock_acc,
    Params = #{jid => UserJid},
    Extra = #{host_type => HostType},

    %% This would trigger our unset_presence/3 function
    Result = unset_presence_simulation(Acc, Params, Extra),

    case Result of
        {ok, _} ->
            io:format("     → Notification sent to world_server@localhost~n");
        Error ->
            io:format("     → Error: ~p~n", [Error])
    end.

show_configuration_examples() ->
    io:format("3. Configuration Examples~n"),
    io:format("   ======================~n"),

    io:format("   Environment Variable Configuration:~n"),
    io:format("   export WORLD_SERVER_USERNAME=\"world_server\"~n~n"),

    io:format("   mongooseim.toml Configuration:~n"),
    io:format("   [modules.mod_presence_notify]~n"),
    io:format("   # Enable the module (no additional config needed)~n~n"),

    io:format("   [modules.mod_presence_notify]~n"),
    io:format("   server_user = \"custom_server\"  # Override env var~n~n"),

    io:format("   Example notification message structure:~n"),
    show_example_message().

show_example_message() ->
    MockJid = {jid, <<"alice">>, <<"localhost">>, <<"mobile">>,
               <<"alice">>, <<"localhost">>, <<"mobile">>},

    Message = create_notification_message_mock(MockJid, <<"available">>),

    io:format("   <message type='chat' id='~s'>~n",
              [proplists:get_value(<<"id">>, Message#xmlel.attrs)]),
    io:format("     <body>Presence update</body>~n"),
    io:format("     <event xmlns='flux:xmpp' mime='application/json'>~n"),
    io:format("       {~n"),
    io:format("         \"trace\": \"<message-id>\",~n"),
    io:format("         \"type\": \"PRESENCE_DID_CHANGE\",~n"),
    io:format("         \"args\": {~n"),
    io:format("           \"user\": \"alice@localhost/mobile\",~n"),
    io:format("           \"presence\": \"available\"~n"),
    io:format("         }~n"),
    io:format("       }~n"),
    io:format("     </event>~n"),
    io:format("   </message>~n").

%% Simulation functions (mock implementations)
start_simulation(_HostType, _Opts) ->
    ok.

hooks_simulation(HostType) ->
    [
     {user_available, HostType, fun user_available_simulation/3, #{}, 90},
     {unset_presence, HostType, fun unset_presence_simulation/3, #{}, 90}
    ].

config_spec_simulation() ->
    #{
        items => #{<<"server_user">> => #{type => binary, validate => non_empty}},
        defaults => #{<<"server_user">> => get_default_server_user_mock()}
    }.

supported_features_simulation() ->
    [dynamic_domains].

user_available_simulation(Acc, #{jid := UserJid}, #{host_type := HostType}) ->
    send_presence_notification_simulation(HostType, UserJid, <<"available">>, Acc),
    {ok, Acc}.

unset_presence_simulation(Acc, #{jid := UserJid}, #{host_type := HostType}) ->
    send_presence_notification_simulation(HostType, UserJid, <<"unavailable">>, Acc),
    {ok, Acc}.

send_presence_notification_simulation(HostType, UserJid, PresenceType, _Acc) ->
    ServerUser = get_server_user_simulation(HostType),
    ServerJid = make_server_jid(ServerUser, UserJid),

    %% Create notification message
    _Notification = create_notification_message_mock(UserJid, PresenceType),

    %% In real implementation, this would route the message
    %% ejabberd_router:route(FromJid, ToJid, Acc, Stanza)

    ok.

get_server_user_simulation(_HostType) ->
    %% In real implementation: gen_mod:get_module_opt(HostType, mod_presence_notify, server_user)
    get_default_server_user_mock().

get_default_server_user_mock() ->
    case os:getenv("WORLD_SERVER_USERNAME") of
        false -> <<"server">>;
        Value -> list_to_binary(Value)
    end.

make_server_jid(ServerUser, {jid, _User, Server, _Resource, _LUser, _LServer, _LResource}) ->
    {jid, ServerUser, Server, <<>>, ServerUser, Server, <<>>}.

create_notification_message_mock(UserJid, PresenceType) ->
    MessageId = generate_message_id_mock(),
    UserJidBin = jid_to_binary_mock(UserJid),

    JsonPayload = #{
        <<"trace">> => MessageId,
                 <<"type">> => <<"PRESENCE_DID_CHANGE">>,
        <<"args">> => #{
            <<"user">> => UserJidBin,
            <<"presence">> => PresenceType
        }
    },

    JsonBinary = encode_json_mock(JsonPayload),

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

generate_message_id_mock() ->
    Timestamp = integer_to_binary(erlang:system_time(microsecond)),
    <<"msg-", Timestamp/binary>>.

jid_to_binary_mock({jid, User, Server, Resource, _LUser, _LServer, _LResource}) ->
    case Resource of
        <<>> -> <<User/binary, "@", Server/binary>>;
        _ -> <<User/binary, "@", Server/binary, "/", Resource/binary>>
    end.

encode_json_mock(_JsonMap) ->
    <<"{\"mock\":\"json\"}">>.
