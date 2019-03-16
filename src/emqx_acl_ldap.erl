%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_acl_ldap).

-include_lib("emqx/include/emqx.hrl").
-include_lib("eldap/include/eldap.hrl").

-export([check_acl/5, reload_acl/1, description/0]).

-import(proplists, [get_value/2]).

-import(lists, [concat/1]).

-import(emqx_auth_ldap_cli, [search/3, init_args/1]).

-type no_match_action() :: atom().

check_acl(#{username := <<$$, _/binary>>}, _PubSub, _Topic, _NoMatchAction, _State) ->
    ok;

check_acl(#{username := Username}, PubSub, Topic, NoMatchAction, 
          #{device_dn := DeviceDn,
            match_objectclass := ObjectClass,
            username_attr := UidAttr}) ->
    Filter = eldap2:equalityMatch("objectClass", ObjectClass),
    Attribute = case PubSub of
                    publish   -> "mqttPublishTopic";
                    subscribe -> "mqttSubscriptionTopic"
                end,
    Attribute1 = "mqttPubSubTopic",
    logger:debug("search dn:~p filter:~p, attribute:~p", [DeviceDn, Filter, Attribute]),
    case search(concat([UidAttr,"=", binary_to_list(Username), ",", DeviceDn]), Filter, [Attribute, Attribute1]) of
        {error, noSuchObject} ->
            ok;
        {ok, #eldap_search_result{entries = []}} ->
            ok;
        {ok, #eldap_search_result{entries = [Entry]}} ->
            Topics = get_value(Attribute, Entry#eldap_entry.attributes)
                ++ get_value(Attribute1, Entry#eldap_entry.attributes),
            match(Topic, Topics, NoMatchAction);
        Error ->
            logger:error("LDAP search error:~p", [Error]),
            {stop, deny}
    end.

match(_Topic, []) ->
    ok;

match(Topic, [Filter | Topics], NoMatchAction) ->
    case emqx_topic:match(Topic, list_to_binary(Filter)) of
        true  -> {stop, allow};
        false -> match(Topic, Topics)
    end.

reload_acl(_State) ->
    ok.

description() ->
    "ACL with LDAP".

