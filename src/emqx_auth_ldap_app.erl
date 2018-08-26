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

-module(emqx_auth_ldap_app).

-behaviour(application).

-include("emqx_auth_ldap.hrl").

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_auth_ldap_sup:start_link(),
    if_enabled(auth_dn, fun reg_authmod/1),
    if_enabled(acl_dn,  fun reg_aclmod/1),
    emqx_auth_ldap_cfg:register(),
    {ok, Sup}.

prep_stop(State) ->
    emqx_access_control:unregister_mod(auth, emqx_auth_ldap),
    emqx_access_control:unregister_mod(acl, emqx_acl_ldap),
    emqx_auth_ldap_cfg:unregister(),
    State.

stop(_State) ->
    ok.

reg_authmod(AuthDn) ->
    {ok, HashType} = application:get_env(?APP, password_hash),
    AuthEnv = {AuthDn, HashType},
    emqx_access_control:register_mod(auth, emqx_auth_ldap, AuthEnv).

reg_aclmod(AclDn) ->
    emqx_access_control:register_mod(acl, emqx_acl_ldap, AclDn).

if_enabled(Cfg, Fun) ->
    case application:get_env(?APP, Cfg) of
        {ok, Dn} -> Fun(Dn);
        undefined -> ok
    end.

