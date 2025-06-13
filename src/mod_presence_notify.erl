%%%----------------------------------------------------------------------
%%% File    : mod_presence_notify.erl
%%% Author  : Industry Digital PTE Ltd
%%% Purpose : Notify a designated user of all presence changes without presence subscriptions
%%% Created : Custom module for World Server presence notifications
%%%----------------------------------------------------------------------

-module(mod_presence_notify).
-author("eng@industrydigital.dev").

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("jlib.hrl").
-include("mongoose_logger.hrl").
-include("mongoose_config_spec.hrl").

%% gen_mod callbacks
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0]).

%% Hook callbacks
-export([user_available/3, unset_presence/3]).

%% Internal functions
-export([send_presence_notification/4, should_notify_for_jid/1]).

-define(DEFAULT_SERVER_USER, <<"server">>).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    ?LOG_INFO(#{what => presence_notify_start, host_type => HostType}),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ?LOG_INFO(#{what => presence_notify_stop, host_type => HostType}),
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [
     {user_available, HostType, fun ?MODULE:user_available/3, #{}, 90},
     {unset_presence, HostType, fun ?MODULE:unset_presence/3, #{}, 90}
    ].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{},
       defaults = #{}
    }.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

%%====================================================================
%% Hook callbacks
%%====================================================================

-spec user_available(mongoose_acc:t(), map(), gen_hook:extra()) ->
    gen_hook:hook_fn_ret(mongoose_acc:t()).
user_available(Acc, #{jid := UserJid}, #{host_type := HostType}) ->
    case should_notify_for_jid(UserJid) of
        true ->
            send_presence_notification(HostType, UserJid, <<"available">>, Acc);
        false ->
            ?LOG_DEBUG(#{what => presence_notify_filtered,
                        user_jid => jid:to_binary(UserJid),
                        reason => <<"JID does not match flux.actor.* or flux.sim.* pattern">>})
    end,
    {ok, Acc}.

-spec unset_presence(mongoose_acc:t(), map(), gen_hook:extra()) ->
    gen_hook:hook_fn_ret(mongoose_acc:t()).
unset_presence(Acc, #{jid := UserJid}, #{host_type := HostType}) ->
    case should_notify_for_jid(UserJid) of
        true ->
            send_presence_notification(HostType, UserJid, <<"unavailable">>, Acc);
        false ->
            ?LOG_DEBUG(#{what => presence_notify_filtered,
                        user_jid => jid:to_binary(UserJid),
                        reason => <<"JID does not match flux.actor.* or flux.sim.* pattern">>})
    end,
    {ok, Acc}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec should_notify_for_jid(jid:jid()) -> boolean().
should_notify_for_jid(UserJid) ->
    LocalPart = jid:luser(UserJid),
    case LocalPart of
        <<"flux.actor.", _/binary>> -> true;
        <<"flux.sim.", _/binary>> -> true;
        _ -> false
    end.

-spec send_presence_notification(mongooseim:host_type(), jid:jid(), binary(), mongoose_acc:t()) -> ok.
send_presence_notification(HostType, UserJid, PresenceType, _Acc) ->
    try
        ServerUser = get_server_user(HostType),
        {_, LServer} = jid:to_lus(UserJid),
        ServerJid = jid:make_noprep(ServerUser, LServer, <<>>),

        % Create the notification message with structured JSON payload
        Notification = create_notification_message(UserJid, PresenceType),

        % Route the notification to the server user
        % ejabberd_router:route will create the mongoose_acc automatically
        ejabberd_router:route(jid:to_bare(UserJid), ServerJid, Notification),

        ?LOG_DEBUG(#{what => presence_notification_sent,
                    user_jid => jid:to_binary(UserJid),
                    presence_type => PresenceType,
                    server_user => jid:to_binary(ServerJid)})
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => presence_notify_error,
                        user_jid => jid:to_binary(UserJid),
                        presence_type => PresenceType,
                        class => Class,
                        reason => Reason,
                        stacktrace => Stacktrace})
    end.

-spec create_notification_message(jid:jid(), binary()) -> exml:element().
create_notification_message(UserJid, PresenceType) ->
    MessageId = mongoose_bin:gen_from_crypto(),
    UserJidBin = jid:to_binary(UserJid),

    % Create structured JSON payload using the message ID as trace
    JsonPayload = #{
        <<"trace">> => MessageId,
        <<"type">> => <<"PRESENCE_DID_CHANGE">>,
        <<"args">> => #{
            <<"user">> => UserJidBin,
            <<"presence">> => PresenceType
        }
    },

    % Safely encode JSON with error handling
    JsonBinary = try
        jiffy:encode(JsonPayload)
    catch
        _:_ ->
            % Fallback if jiffy fails
            <<"{'trace':'", MessageId/binary, "','type':'PRESENCE_DID_CHANGE','args':{'user':'",
              UserJidBin/binary, "','presence':'", PresenceType/binary, "'}}">>
    end,

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

-spec get_server_user(mongooseim:host_type()) -> binary().
get_server_user(_HostType) ->
    get_default_server_user().

-spec get_default_server_user() -> binary().
get_default_server_user() ->
    case os:getenv("WORLD_SERVER_USERNAME") of
        false -> ?DEFAULT_SERVER_USER;
        Value -> list_to_binary(Value)
    end.
