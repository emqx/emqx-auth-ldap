%%--------------------------------------------------------------------
%% Copyright (c) 2013-2018 EMQ Enterprise, Inc. (http://emqtt.io)
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
%%--------------------------------------------------------------------

-module(emq_auth_ldap_cli).

-behaviour(ecpool_worker).

-include("emq_auth_ldap.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-import(proplists, [get_value/2, get_value/3]).

-export([connect/1, search/2, fill/2, gen_filter/2]).

fill(Client, AuthDn) ->
    case re:run(AuthDn, "%[uc]", [global, {capture, all, list}]) of
        {match, [["%u"]]} ->
            re:replace(AuthDn, "%u", binary_to_list(Client#mqtt_client.username), [global, {return, list}]);
        {match, [["%c"]]} ->
            re:replace(AuthDn, "%c", binary_to_list(Client#mqtt_client.client_id), [global, {return, list}]);
        nomatch ->
            AuthDn
    end.

gen_filter(Client, Dn) ->
    case re:run(Dn, "%[uc]", [global, {capture, all, list}]) of
        {match, [["%u"]]} -> eldap:equalityMatch("username", Client#mqtt_client.username);
        {match, [["%c"]]} -> eldap:equalityMatch("username", Client#mqtt_client.client_id);
        nomatch           -> eldap:equalityMatch("username", Client#mqtt_client.username)
    end.

%%--------------------------------------------------------------------
%% ldap Connect/Search
%%--------------------------------------------------------------------
connect(Opts) ->
    Servers      = get_value(servers, Opts, ["localhost"]),
    Port         = get_value(port, Opts, 389),
    Timeout      = get_value(timeout, Opts, 30),
    BindDn       = get_value(bind_dn, Opts),
    BindPassword = get_value(bind_password, Opts),
    LdapOpts = case get_value(ssl, Opts, false) of
        true -> 
            SslOpts = get_value(sslopts, Opts),
            [{port, Port}, {timeout, Timeout}, {sslopts, SslOpts}];
        false ->
            [{port, Port}, {timeout, Timeout}]
    end,

    case eldap:open(Servers, LdapOpts) of
        {ok, LDAP} ->
            case catch eldap:simple_bind(LDAP, BindDn, BindPassword) of
                ok ->
                    {ok, LDAP};
                {error, Error} ->
                    {error, Error};
                {'EXIT', Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

search(Base, Filter) ->
    ecpool:with_client(?APP, fun(C) -> eldap:search(C, [{base, Base}, {filter, Filter}]) end).
    
