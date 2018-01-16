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

-module(emq_auth_ldap_app).

-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

-include("emq_auth_ldap.hrl").

%%--------------------------------------------------------------------
%% Application callbacks
%%--------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    {ok, Sup} = emq_auth_ldap_sup:start_link(),
    if_enabled(auth_dn, fun reg_authmod/1),
    emq_auth_ldap_config:register(),
    {ok, Sup}.

prep_stop(State) ->
    emqttd_access_control:unregister_mod(auth, emq_auth_ldap),
    emq_auth_ldap_config:unregister(),
    State.

stop(_State) ->
    ok.

reg_authmod(AuthDn) ->
    {ok, HashType} = application:get_env(?APP, password_hash),
    AuthEnv = {AuthDn, HashType},
    emqttd_access_control:register_mod(auth, emq_auth_ldap, AuthEnv).


%%--------------------------------------------------------------------
%% Internal function
%%--------------------------------------------------------------------

if_enabled(Cfg, Fun) ->
    case application:get_env(?APP, Cfg) of
        {ok, Dn} -> Fun(Dn);
        undefined   -> ok
    end.

