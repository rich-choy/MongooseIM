%%%-------------------------------------------------------------------
%%% @copyright (C) 2025
%%% This software is released under the Apache License, Version 2.0
%%% cited in 'LICENSE.txt'.
%%% @end
%%%-------------------------------------------------------------------
%%% @doc
%%% Ephemeral event bus node implementation for PubSub.
%%%
%%% This node type provides zero-persistence event broadcasting:
%%% - No database writes or item storage
%%% - Immediate broadcast to all subscribers
%%% - Designed for high-throughput ephemeral events
%%% - Future support for multi-item publishing
%%%
%%% Unlike node_push (which triggers external hooks), this plugin
%%% uses standard PubSub broadcast semantics to deliver events
%%% directly to subscribed XMPP clients.
%%% @end
%%%-------------------------------------------------------------------
-module(node_eventbus).
-behaviour(gen_pubsub_node).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("pubsub.hrl").

-export([based_on/0, init/3, terminate/2, options/0, features/0,
         publish_item/9, node_to_path/1, should_delete_when_owner_removed/0]).

%%====================================================================
%% gen_pubsub_node callbacks
%%====================================================================

%% @doc Inherit standard subscription and affiliation management from node_flat
based_on() -> node_flat.

%% @doc Initialize the eventbus node type
init(Host, ServerHost, Opts) ->
    node_flat:init(Host, ServerHost, Opts),
    ok.

%% @doc Cleanup on termination
terminate(Host, ServerHost) ->
    node_flat:terminate(Host, ServerHost),
    ok.

%% @doc Node configuration options
%% Key differences from node_flat:
%% - persist_items: false (no database writes)
%% - max_items: 0 (no item storage/retrieval)
%% - publish_model: open (any subscriber can publish)
%% - access_model: open (anyone can subscribe)
options() ->
    [{deliver_payloads, true},
     {notify_config, false},
     {notify_delete, false},
     {notify_retract, false},
     {purge_offline, false},
     {persist_items, false},           % No persistence
     {max_items, 0},                   % No storage
     {subscribe, true},
     {access_model, open},             % Open subscription
     {roster_groups_allowed, []},
     {publish_model, open},            % Open publishing
     {notification_type, headline},
     {max_payload_size, ?MAX_PAYLOAD_SIZE},
     {send_last_published_item, never}, % No items stored
     {deliver_notifications, true},
     {presence_based_delivery, false}].

%% @doc Advertised features for service discovery
features() ->
    [
        <<"create-nodes">>,
        <<"delete-nodes">>,
        <<"modify-affiliations">>,
        <<"publish">>,
        <<"purge-nodes">>,
        <<"retrieve-affiliations">>
    ].

%% @doc Publish an item to the eventbus
%% This is the core function that differs from node_push:
%% - Accepts any payload (not just <notification> elements)
%% - Returns {default, broadcast, []} to trigger standard PubSub broadcast
%% - Performs no database operations
%% - Validates publisher permissions only
%%
%% Return value semantics:
%% {result, {default, broadcast, []}} means:
%%   - default: use default response formatting
%%   - broadcast: trigger broadcast_publish_item in mod_pubsub.erl:2272
%%   - []: no items to retract (empty list)
-spec publish_item(ServerHost :: jid:server(),
                   Nidx :: mod_pubsub:nodeIdx(),
                   Publisher :: jid:jid(),
                   PublishModel :: mod_pubsub:publishModel(),
                   MaxItems :: non_neg_integer(),
                   ItemId :: binary() | mod_pubsub:itemId(),
                   ItemPublisher :: boolean(),
                   Payload :: mod_pubsub:payload(),
                   PublishOptions :: mod_pubsub:publishOptions()) ->
    {result, {default, broadcast, []}} | {error, exml:element()}.
publish_item(_ServerHost, Nidx, Publisher, PublishModel, _MaxItems, _ItemId,
             _ItemPublisher, _Payload, _PublishOptions) ->
    %% Get publisher's affiliation for permission check
    {ok, Affiliation} = mod_pubsub_db_backend:get_affiliation(Nidx, jid:to_lower(Publisher)),

    %% Check if publisher is allowed based on publish model and affiliation
    case is_allowed_to_publish(PublishModel, Affiliation) of
        true ->
            %% Return broadcast tuple to trigger notification to subscribers
            %% The payload will be broadcast by mod_pubsub:broadcast_publish_item/10
            {result, {default, broadcast, []}};
        false ->
            {error, mongoose_xmpp_errors:forbidden()}
    end.

%% @doc Convert node to path (delegate to node_flat)
node_to_path(Node) ->
    node_flat:node_to_path(Node).

%% @doc Delete eventbus nodes when owner is removed
should_delete_when_owner_removed() -> true.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Check if publisher is allowed to publish based on model and affiliation
%% @private
-spec is_allowed_to_publish(PublishModel :: mod_pubsub:publishModel(),
                            Affiliation :: mod_pubsub:affiliation()) -> boolean().
is_allowed_to_publish(open, _Affiliation) ->
    %% Open model: anyone can publish
    true;
is_allowed_to_publish(publishers, Affiliation) ->
    %% Publishers model: only owners, publishers, and publish_only can publish
    (Affiliation == owner)
        orelse (Affiliation == publisher)
        orelse (Affiliation == publish_only);
is_allowed_to_publish(subscribers, _Affiliation) ->
    %% Subscribers model: would need to check subscription status
    %% For now, delegate to node_flat's more complex logic if needed
    %% This is a simplified version - could be enhanced
    false.
