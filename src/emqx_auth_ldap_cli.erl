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

-module(emqx_auth_ldap_cli).

-behaviour(ecpool_worker).

-include("emqx_auth_ldap.hrl").

-include_lib("emqx/include/emqx.hrl").

-import(proplists, [get_value/2, get_value/3]).

%% ecpool callback
-export([connect/1]).

-export([search/2, search/3, init_args/1]).

%%--------------------------------------------------------------------
%% LDAP Connect/Search
%%--------------------------------------------------------------------

connect(Opts) ->
    Servers      = get_value(servers, Opts, ["localhost"]),
    Port         = get_value(port, Opts, 389),
    Timeout      = get_value(timeout, Opts, 30),
    BindDn       = get_value(bind_dn, Opts),
    BindPassword = get_value(bind_password, Opts),
    LdapOpts     = case get_value(ssl, Opts, false) of
                       true ->
                           SslOpts = get_value(sslopts, Opts),
                           [{port, Port}, {timeout, Timeout}, {sslopts, SslOpts}];
                       false ->
                           [{port, Port}, {timeout, Timeout}]
                   end,
    logger:debug("Connecting to OpenLDAP server: ~p, Opts:~p ...", [Servers, LdapOpts]),
    case eldap2:open(Servers, LdapOpts) of
        {ok, LDAP} ->
            try eldap2:simple_bind(LDAP, BindDn, BindPassword) of
                ok -> 
                    {ok, LDAP};
                {error, Error} ->
                    {error, Error}
            catch
                error : Reason ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

search(Base, Filter) ->
    ecpool:with_client(?APP, fun(C) -> 
        eldap2:search(C, [{base, Base}, 
                         {filter, Filter},
                         {deref, eldap2:derefFindingBaseObj()}]) 
    end).

search(Base, Filter, Attributes) ->
    ecpool:with_client(?APP, fun(C) -> 
                                 eldap2:search(C, [{base, Base},
                                                   {filter, Filter},
                                                   {attributes, Attributes},
                                                   {deref, eldap2:derefFindingBaseObj()}]) 
                             end).

init_args(ENVS) ->
    DeviceDn = get_value(device_dn, ENVS),
    ObjectClass = get_value(match_objectclass, ENVS),
    UidAttr = get_value(username_attr, ENVS),
    PasswdAttr = get_value(password_attr, ENVS),
    {ok, #{device_dn => DeviceDn,
           match_objectclass => ObjectClass,
           username_attr => UidAttr,
           password_attr => PasswdAttr}}.
