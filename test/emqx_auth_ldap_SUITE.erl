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

-module(emqx_auth_ldap_SUITE).

-compile(export_all).

-compile(no_warning_export).

-define(PID, emqx_auth_ldap).

-define(APP, emqx_auth_ldap).

-define(DeviceDN, "ou=test_device,dc=emqx,dc=io").

-define(AuthDN, "ou=test_auth,dc=emqx,dc=io").

-include_lib("emqx/include/emqx.hrl").

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

all() ->
    [
     check_auth,
     check_acl
    ].

init_per_suite(Config) ->
    [start_apps(App, SchemaFile, ConfigFile) ||
        {App, SchemaFile, ConfigFile}
            <- [{emqx, deps_path(emqx, "priv/emqx.schema"),
                       deps_path(emqx, "etc/emqx.conf")},
                {emqx_auth_ldap, local_path("priv/emqx_auth_ldap.schema"),
                                 local_path("etc/emqx_auth_ldap.conf")}]],
    Config.

end_per_suite(_Config) ->
    [application:stop(App) || App <- [emqx_auth_ldap, emqx]].

check_auth(_) ->
    MqttUser1 = #{client_id => <<"mqttuser1">>, username => <<"mqttuser0001">>},
    MqttUser2 = #{client_id => <<"mqttuser2">>, username => <<"mqttuser0002">>},
    MqttUser3 = #{client_id => <<"mqttuser3">>, username => <<"mqttuser0003">>},
    MqttUser4 = #{client_id => <<"mqttuser4">>, username => <<"mqttuser0004">>},
    MqttUser5 = #{client_id => <<"mqttuser5">>, username => <<"mqttuser0005">>},
    NonExistUser1 = #{client_id => <<"mqttuser6">>, username => <<"mqttuser0006">>},
    NonExistUser2 = #{client_id => <<"mqttuser7">>, username => <<"mqttuser0005">>},

    ok = emqx_access_control:authenticate(MqttUser1, <<"mqttuser0001">>),

    ok = emqx_access_control:authenticate(MqttUser2, <<"mqttuser0002">>),

    ok = emqx_access_control:authenticate(MqttUser3, <<"mqttuser0003">>),

    ok = emqx_access_control:authenticate(MqttUser4, <<"mqttuser0004">>),

    ok = emqx_access_control:authenticate(MqttUser5, <<"mqttuser0005">>),
    
    {error, auth_modules_not_found} = emqx_access_control:authenticate(NonExistUser1, <<"mqttuser0006">>),

    {error, password_error} = emqx_access_control:authenticate(NonExistUser2, <<"mqttuser0006">>).

check_acl(_) ->
    MqttUser = #{client_id => <<"mqttuser1">>, username => <<"mqttuser0001">>, zone => undefined},
    NoMqttUser = #{client_id => <<"mqttuser2">>, username => <<"mqttuser0007">>, zone => undefined},
    allow = emqx_access_control:check_acl(MqttUser, publish, <<"/mqttuser0001/pub/1">>),
    allow = emqx_access_control:check_acl(MqttUser, publish, <<"/mqttuser0001/pub/+">>),
    allow = emqx_access_control:check_acl(MqttUser, publish, <<"/mqttuser0001/pub/#">>),

    allow = emqx_access_control:check_acl(MqttUser, subscribe, <<"/mqttuser0001/sub/1">>),
    allow = emqx_access_control:check_acl(MqttUser, subscribe, <<"/mqttuser0001/sub/+">>),
    allow = emqx_access_control:check_acl(MqttUser, subscribe, <<"/mqttuser0001/sub/#">>),

    allow = emqx_access_control:check_acl(MqttUser, publish, <<"/mqttuser0001/pubsub/1">>),
    allow = emqx_access_control:check_acl(MqttUser, publish, <<"/mqttuser0001/pubsub/+">>),
    allow = emqx_access_control:check_acl(MqttUser, publish, <<"/mqttuser0001/pubsub/#">>),
    allow = emqx_access_control:check_acl(MqttUser, subscribe, <<"/mqttuser0001/pubsub/1">>),
    allow = emqx_access_control:check_acl(MqttUser, subscribe, <<"/mqttuser0001/pubsub/+">>),
    allow = emqx_access_control:check_acl(MqttUser, subscribe, <<"/mqttuser0001/pubsub/#">>),

    deny = emqx_access_control:check_acl(NoMqttUser, publish, <<"/mqttuser0001/req/mqttuser0001/+">>),
    deny = emqx_access_control:check_acl(MqttUser, publish, <<"/mqttuser0001/req/mqttuser0002/+">>),
    deny = emqx_access_control:check_acl(MqttUser, subscribe, <<"/mqttuser0001/req/+/mqttuser0002">>),
    ok.
    
start_apps(App, SchemaFile, ConfigFile) ->
    read_schema_configs(App, SchemaFile, ConfigFile),
    set_special_configs(App),
    application:ensure_all_started(App).

read_schema_configs(App, SchemaFile, ConfigFile) ->
    ct:pal("Read configs - SchemaFile: ~p, ConfigFile: ~p", [SchemaFile, ConfigFile]),
    Schema = cuttlefish_schema:files([SchemaFile]),
    Conf = conf_parse:file(ConfigFile),
    NewConfig = cuttlefish_generator:map(Schema, Conf),
    Vals = proplists:get_value(App, NewConfig, []),
    [application:set_env(App, Par, Value) || {Par, Value} <- Vals].

set_special_configs(emqx) ->
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, enable_acl_cache, false),
    application:set_env(emqx, acl_nomatch, deny),
    application:set_env(emqx, plugins_loaded_file, deps_path(emqx, "test/emqx_SUITE_data/loaded_plugins"));

set_special_configs(emqx_auth_ldap) ->
    application:set_env(emqx_auth_ldap, device_dn, "ou=testdevice, dc=emqx, dc=io");
set_special_configs(_App) ->
    ok.

local_path(RelativePath) ->
    deps_path(emqx_auth_ldap, RelativePath).

deps_path(App, RelativePath) ->
    Path0 = code:priv_dir(App),
    Path = case file:read_link(Path0) of
               {ok, Resolved} -> Resolved;
               {error, _} -> Path0
            end,
    filename:join([Path, "..", RelativePath]).
