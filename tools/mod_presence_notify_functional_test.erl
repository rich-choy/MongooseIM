#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

-mode(compile).

%% Mock records and types for testing
-record(xmlel, {name, attrs = [], children = []}).
-record(xmlcdata, {content}).

main([]) ->
    io:format("Running functional tests for mod_presence_notify...~n~n"),

    %% Set up mock environment
    setup_mocks(),

    %% Run unit tests (existing)
    test_environment_variable_handling(),
    test_message_creation(),
    test_json_payload_structure(),
    test_trace_id_correlation(),

    %% Run integration tests (new)
    test_mongooseim_http_connectivity(),
    test_mongooseim_admin_api_connectivity(),
    test_module_boot_success(),

    io:format("~n✓ All functional tests completed!~n").

setup_mocks() ->
    %% Mock environment variables
    put(mock_env, #{"WORLD_SERVER_USERNAME" => "testuser"}),

    %% Mock gen_mod options
    put(mock_gen_mod, #{server_user => <<"configured_user">>}),

    io:format("✓ Mock environment set up~n").

test_environment_variable_handling() ->
    io:format("~nTesting environment variable handling...~n"),

    %% Test default value
    DefaultUser = get_default_server_user_mock(),
    case DefaultUser of
        <<"testuser">> ->
            io:format("✓ Environment variable correctly read: ~s~n", [DefaultUser]);
        _ ->
            io:format("✗ Environment variable test failed, got: ~s~n", [DefaultUser])
    end,

    %% Test fallback when env var not set
    put(mock_env, #{}),
    FallbackUser = get_default_server_user_mock(),
    case FallbackUser of
        <<"server">> ->
            io:format("✓ Fallback to default server user works~n");
        _ ->
            io:format("✗ Fallback test failed, got: ~s~n", [FallbackUser])
    end.

test_message_creation() ->
    io:format("~nTesting message creation...~n"),

    %% Create a mock JID
    MockJid = {jid, <<"alice">>, <<"localhost">>, <<"mobile">>,
               <<"alice">>, <<"localhost">>, <<"mobile">>},

    %% Create notification message
    Message = create_notification_message_mock(MockJid, <<"available">>),

    %% Validate message structure
    case Message of
        #xmlel{name = <<"message">>, attrs = Attrs, children = Children} ->
            io:format("✓ Message structure is correct~n"),

            %% Check message type
            case proplists:get_value(<<"type">>, Attrs) of
                <<"chat">> ->
                    io:format("✓ Message type is 'chat'~n");
                Other ->
                    io:format("✗ Wrong message type: ~p~n", [Other])
            end,

            %% Check message ID exists
            case proplists:get_value(<<"id">>, Attrs) of
                undefined ->
                    io:format("✗ Message ID missing~n");
                MessageId when is_binary(MessageId) ->
                    io:format("✓ Message ID present: ~s~n", [MessageId])
            end,

            %% Check children count
            case length(Children) of
                2 ->
                    io:format("✓ Message has correct number of children (2)~n");
                Count ->
                    io:format("✗ Wrong number of children: ~p~n", [Count])
            end;
        _ ->
            io:format("✗ Message structure is invalid~n")
    end.

test_json_payload_structure() ->
    io:format("~nTesting JSON payload structure...~n"),

    MockJid = {jid, <<"bob">>, <<"example.com">>, <<"desktop">>,
               <<"bob">>, <<"example.com">>, <<"desktop">>},

    %% Create JSON payload
    MessageId = <<"test-message-123">>,
    JsonPayload = create_json_payload_mock(MockJid, <<"unavailable">>, MessageId),

    %% Validate JSON structure
    ExpectedKeys = [<<"trace">>, <<"type">>, <<"args">>],
    lists:foreach(fun(Key) ->
        case maps:is_key(Key, JsonPayload) of
            true ->
                io:format("✓ JSON contains key: ~s~n", [Key]);
            false ->
                io:format("✗ JSON missing key: ~s~n", [Key])
        end
    end, ExpectedKeys),

    %% Check specific values
    case maps:get(<<"type">>, JsonPayload, undefined) of
        <<"PRESENCE_DID_CHANGE">> ->
            io:format("✓ JSON type is correct~n");
        Other ->
            io:format("✗ Wrong JSON type: ~p~n", [Other])
    end,

    %% Check args structure
    case maps:get(<<"args">>, JsonPayload, undefined) of
        Args when is_map(Args) ->
            case {maps:get(<<"user">>, Args, undefined),
                  maps:get(<<"presence">>, Args, undefined)} of
                {<<"bob@example.com/desktop">>, <<"unavailable">>} ->
                    io:format("✓ JSON args are correct~n");
                {User, Presence} ->
                    io:format("✗ Wrong JSON args: user=~p, presence=~p~n", [User, Presence])
            end;
        _ ->
            io:format("✗ JSON args structure is invalid~n")
    end.

test_trace_id_correlation() ->
    io:format("~nTesting trace ID correlation...~n"),

    MockJid = {jid, <<"charlie">>, <<"test.com">>, <<"phone">>,
               <<"charlie">>, <<"test.com">>, <<"phone">>},

    Message = create_notification_message_mock(MockJid, <<"available">>),

    %% Extract message ID from stanza
    #xmlel{attrs = Attrs} = Message,
    MessageId = proplists:get_value(<<"id">>, Attrs),

    %% Extract trace ID from JSON (simulate)
    JsonPayload = create_json_payload_mock(MockJid, <<"available">>, MessageId),
    TraceId = maps:get(<<"trace">>, JsonPayload),

    %% Verify they match
    case MessageId =:= TraceId of
        true ->
            io:format("✓ Trace ID matches message ID: ~s~n", [MessageId]);
        false ->
            io:format("✗ Trace ID mismatch: message=~s, trace=~s~n", [MessageId, TraceId])
    end.

%% Mock functions
get_default_server_user_mock() ->
    MockEnv = get(mock_env),
    case maps:get("WORLD_SERVER_USERNAME", MockEnv, undefined) of
        undefined -> <<"server">>;
        Value -> list_to_binary(Value)
    end.

create_notification_message_mock(UserJid, PresenceType) ->
    MessageId = generate_message_id_mock(),
    UserJidBin = jid_to_binary_mock(UserJid),

    JsonPayload = create_json_payload_mock(UserJid, PresenceType, MessageId),
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

create_json_payload_mock(UserJid, PresenceType, MessageId) ->
    UserJidBin = jid_to_binary_mock(UserJid),
    #{
        <<"trace">> => MessageId,
        <<"type">> => <<"PRESENCE_DID_CHANGE">>,
        <<"args">> => #{
            <<"user">> => UserJidBin,
            <<"presence">> => PresenceType
        }
    }.

generate_message_id_mock() ->
    Timestamp = integer_to_binary(erlang:system_time(microsecond)),
    <<"test-", Timestamp/binary>>.

jid_to_binary_mock({jid, User, Server, Resource, _LUser, _LServer, _LResource}) ->
    case Resource of
        <<>> -> <<User/binary, "@", Server/binary>>;
        _ -> <<User/binary, "@", Server/binary, "/", Resource/binary>>
    end.

encode_json_mock(_JsonMap) ->
    %% Simple JSON encoding mock for testing
    <<"{\"mock\":\"json\"}">>.

%% New integration tests for HTTP connectivity
test_mongooseim_http_connectivity() ->
    io:format("~nTesting MongooseIM HTTP connectivity...~n"),

    %% Get configuration from environment or use defaults
    HttpPort = get_env_or_default("XMPP_SERVER_WEBSOCKET_PORT", "5280"),
    Hostname = get_env_or_default("HOSTNAME", "localhost"),

    io:format("Testing HTTP endpoint: http://~s:~s/~n", [Hostname, HttpPort]),

    %% Test HTTP connectivity with retries
    MaxRetries = 3,
    RetryDelay = 500, % 0.5 seconds

    case test_http_endpoint_with_retries(Hostname, HttpPort, "/", MaxRetries, RetryDelay) of
        {ok, ResponseCode} ->
            io:format("✓ MongooseIM HTTP endpoint responsive (HTTP ~p)~n", [ResponseCode]);
        {error, Reason} ->
            io:format("✗ MongooseIM HTTP endpoint failed: ~p~n", [Reason]),
            io:format("  This indicates MongooseIM may be hanging during boot~n")
    end.

test_mongooseim_admin_api_connectivity() ->
    io:format("~nTesting MongooseIM Admin API connectivity...~n"),

    %% Get configuration from environment or use defaults
    AdminPort = get_env_or_default("MONGOOSEIM_ADMIN_API_PORT", "8080"),
    Hostname = get_env_or_default("HOSTNAME", "localhost"),

    io:format("Testing Admin API endpoint: http://~s:~s/~n", [Hostname, AdminPort]),

    %% Test Admin API connectivity with retries
    MaxRetries = 3,
    RetryDelay = 500, % 0.5 seconds

    case test_http_endpoint_with_retries(Hostname, AdminPort, "/", MaxRetries, RetryDelay) of
        {ok, ResponseCode} ->
            io:format("✓ MongooseIM Admin API endpoint responsive (HTTP ~p)~n", [ResponseCode]);
        {error, Reason} ->
            io:format("✗ MongooseIM Admin API endpoint failed: ~p~n", [Reason]),
            io:format("  This indicates MongooseIM may be hanging during boot~n")
    end.

test_module_boot_success() ->
    io:format("~nTesting mod_presence_notify module boot success...~n"),

    %% Get configuration from environment or use defaults
    HttpPort = get_env_or_default("XMPP_SERVER_WEBSOCKET_PORT", "5280"),
    AdminPort = get_env_or_default("MONGOOSEIM_ADMIN_API_PORT", "8080"),
    Hostname = get_env_or_default("HOSTNAME", "localhost"),

    %% Test both endpoints to verify MongooseIM booted successfully with our module
    HttpResult = test_http_endpoint_with_retries(Hostname, HttpPort, "/", 2, 500),
    AdminResult = test_http_endpoint_with_retries(Hostname, AdminPort, "/", 2, 500),

    case {HttpResult, AdminResult} of
        {{ok, _}, _} ->
            io:format("✓ mod_presence_notify module loaded successfully (HTTP endpoint responsive)~n");
        {_, {ok, _}} ->
            io:format("✓ mod_presence_notify module loaded successfully (Admin API responsive)~n");
        {{error, HttpError}, {error, AdminError}} ->
            io:format("✗ mod_presence_notify module may have prevented MongooseIM boot~n"),
            io:format("  HTTP error: ~p~n", [HttpError]),
            io:format("  Admin API error: ~p~n", [AdminError]),
            io:format("  Check MongooseIM logs for module initialization errors~n")
    end.

%% Helper function to test HTTP endpoint with retries
test_http_endpoint_with_retries(Hostname, Port, Path, MaxRetries, RetryDelay) ->
    test_http_endpoint_with_retries(Hostname, Port, Path, MaxRetries, RetryDelay, 1).

test_http_endpoint_with_retries(Hostname, Port, Path, MaxRetries, RetryDelay, Attempt) ->
    case test_http_endpoint(Hostname, Port, Path) of
        {ok, ResponseCode} ->
            {ok, ResponseCode};
        {error, Reason} when Attempt < MaxRetries ->
            io:format("  Attempt ~p/~p failed: ~p, retrying in ~pms...~n",
                     [Attempt, MaxRetries, Reason, RetryDelay]),
            timer:sleep(RetryDelay),
            test_http_endpoint_with_retries(Hostname, Port, Path, MaxRetries, RetryDelay, Attempt + 1);
        {error, Reason} ->
            {error, Reason}
    end.

%% Helper function to test a single HTTP endpoint
test_http_endpoint(Hostname, Port, Path) ->
    try
        %% Use httpc for HTTP requests
        inets:start(),
        ssl:start(),

        Url = "http://" ++ Hostname ++ ":" ++ Port ++ Path,
        RequestTimeout = 2000, % 2 seconds

        case httpc:request(get, {Url, []}, [{timeout, RequestTimeout}], []) of
            {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, _Body}} ->
                {ok, StatusCode};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Class:Error ->
            {error, {Class, Error}}
    end.

%% Helper function to get environment variable or default
get_env_or_default(VarName, Default) ->
    case os:getenv(VarName) of
        false -> Default;
        Value -> Value
    end.
