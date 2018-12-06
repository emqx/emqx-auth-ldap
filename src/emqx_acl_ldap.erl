%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-behaviour(emqx_acl_mod).

-include_lib("emqx/include/emqx.hrl").
-include_lib("eldap/include/eldap.hrl").

%% ACL Callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-import(proplists, [get_value/2, get_value/3]).

-import(lists, [concat/1]).

-import(emqx_auth_ldap_cli, [search/3]).

init(ENVS) ->
    DeviceDn = get_value(device_dn, ENVS, "ou=device,ou=Auth,ou=MQ,dc=emqx,dc=io"),
    ObjectClass = get_value(objectclass, ENVS, "mqttUser"),
    {ok, #{device_dn => DeviceDn,
           objectclass => ObjectClass}}.

check_acl({#{username := <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    ignore;

check_acl({#{username := Username}, PubSub, Topic}, #{device_dn := DeviceDn,
                                                      objectclass := ObjectClass}) ->
    Filter = eldap2:equalityMatch("objectClass", ObjectClass),
    Attribute = case PubSub of
                    publish   -> "mqttPublishTopic";
                    subscribe -> "mqttSubscriptionTopic"
                end,
    logger:debug("search dn:~p filter:~p, attribute:~p", [DeviceDn, Filter, Attribute]),
    case search(concat(["uid=", binary_to_list(Username), ",", DeviceDn]), Filter, [Attribute]) of
        {error, noSuchObject} ->
            ignore;
        {ok, #eldap_search_result{entries = []}} ->
            ignore;
        {ok, #eldap_search_result{entries = [Entry]}} ->
            Topics = get_value(Attribute, Entry#eldap_entry.attributes),
            match(Topic, Topics);
        Error ->
            logger:error("LDAP search error:~p", [Error]),
            deny
    end.

match(_Topic, []) ->
    ignore;

match(Topic, [Filter | Topics]) ->
    case emqx_topic:match(Topic, list_to_binary(Filter)) of
        true  -> allow;
        false -> match(Topic, Topics)
    end.

reload_acl(_State) ->
    ok.

description() ->
    "ACL with LDAP".

