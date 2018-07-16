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

-import(emqx_auth_ldap_cli, [search/2, fill/2, gen_filter/2]).

-record(state, {acl_dn}).

init(AclDn) ->
    {ok, #state{acl_dn = AclDn}}.

check_acl({#mqtt_client{username = <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    ignore;

check_acl({Client, PubSub, Topic}, #state{acl_dn = AclDn}) ->
    Filter = gen_filter(Client, AclDn),
    case search(fill(Client, AclDn), Filter) of
        {ok, #eldap_search_result{entries = []}} ->
            ignore;
        {ok, #eldap_search_result{entries = [Entry]}} ->
            Rules = filter(PubSub, compile(Entry#eldap_entry.attributes)),
            case match(Client, Topic, Rules) of
                {matched, allow} -> allow;
                {matched, deny}  -> deny;
                nomatch          -> ignore
            end
    end.

match(_Client, _Topic, []) ->
    nomatch;

match(Client, Topic, [Rule|Rules]) ->
    case emqx_access_rule:match(Client, Topic, Rule) of
        nomatch -> match(Client, Topic, Rules);
        {matched, AllowDeny} -> {matched, AllowDeny}
    end.

compile(Attributes) ->
    Topic = list_to_binary(get_value("topic", Attributes)),
    Allow  = allow(list_to_binary(get_value("allow", Attributes))),
    Access = access(list_to_binary(get_value("access", Attributes))),
    [emqx_access_rule:compile({Allow, all, Access, [topic(Topic)]})].

filter(PubSub, Rules) ->
    [Term || Term = {_, _, Access, _} <- Rules, Access =:= PubSub orelse Access =:= pubsub].


allow(<<"1">>)  -> allow;
allow(<<"0">>)  -> deny.

access(<<"1">>) -> subscribe;
access(<<"2">>) -> publish;
access(<<"3">>) -> pubsub.

topic(<<"eq ", Topic/binary>>) ->
    {eq, Topic};
topic(Topic) ->
    Topic.

reload_acl(_State) ->
    ok.

description() ->
    "ACL with LDAP".

