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
     {group, emqx_auth_ldap_auth}
     %% {group, emqx_auth_ldap}
    ].

groups() ->
    [{emqx_auth_ldap_auth, [sequence], [check_auth, list_auth]},
     {emqx_auth_ldap, [sequence], [comment_config]}].

init_per_suite(Config) ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(eldap, add, x),
    dbg:tpl(emqx_access_control, authenticate, x),
    dbg:tpl(emqx_auth_ldap, check, x),
    dbg:tpl(emqx_auth_ldap_cli, gen_filter, x),
    dbg:tpl(emqx_auth_ldap_cli, fill, x),
    dbg:tpl(eldap, search, x),
    dbg:tpl(emqx_auth_ldap_SUITE, start_apps,x),

    [start_apps(App, SchemaFile, ConfigFile) ||
        {App, SchemaFile, ConfigFile}
            <- [{emqx, deps_path(emqx, "priv/emqx.schema"),
                       deps_path(emqx, "etc/emqx.conf")},
                {emqx_auth_ldap, local_path("priv/emqx_auth_ldap.schema"),
                                 local_path("etc/emqx_auth_ldap.conf")}]],
    %% prepare(),
    Config.

end_per_suite(_Config) ->
    clean(),
    [application:stop(App) || App <- [emqx_auth_ldap, emqx]].

%% prepare() ->
%%     {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
%%     init_acl(Pid),
%%     init_auth(Pid)
        

clean() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    clean_acl(Pid),
    clean_auth(Pid).

init_acl(_Pid) ->
    ok.

clean_acl(Pid) ->
    %% {ok, _Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})).
    ok.

init_auth(Pid) ->
    %% add origanization info
    eldap:add(Pid, "dc=emqx,dc=io",
                   [{"objectclass", ["dcObject", "organization"]},
                    {"dc", ["emqx"]}, {"o", ["emqx,Inc."]}]),


    %% add origanization unit info
    eldap:add(Pid, ?DeviceDN,
              [{"objectclass", ["organizationalUnit"]},
               {"ou", ["test_device"]}]),

    eldap:add(Pid, "uid=plain," ++ ?DeviceDN,
              [{"uid", ["plain"]},
               {"objectclass", ["mqttUser"]},
               {"userPassword", ["{plain}plain"]}]),

    eldap:add(Pid, "cn=actorcloud," ++ ?DeviceDN,
              [{"cn", ["actorcloud"]},
               {"objectclass", ["eMQTT"]},
               {"uid", ["md5"]},
               {"userPassword", ["{md5}1bc29b36f623ba82aaf6724fd3b16718"]}]),

    eldap:add(Pid, "cn=mqtt," ++ ?DeviceDN,
              [{"cn", ["mqtt"]},
               {"objectclass", ["eMQTT"]},
               {"uid", ["sha"]}, 
               {"userPassword", ["{sha}d8f4590320e1343a915b6394170650a8f35d6926"]}]),
    eldap:add(Pid, "cn=test," ++ ?DeviceDN,
              [{"cn", ["test"]},
               {"objectclass", ["eMQTT"]},
               {"uid", ["sha256"]}, 
               {"userPassword", ["{sha256}5d5b09f6dcb2d53a5fffc60c4ac0d55fabdf556069d6631545f42aa6e3500f2e"]}]).

clean_auth(Pid) ->
    case eldap:search(Pid, [{base, ?AuthDN},
                            {filter, eldap:present("objectclass")},
                            {scope, eldap:wholeSubtree()}])
    of
        {ok, {eldap_search_result, Entries, _}} ->
            [ok = eldap:delete(Pid, Entry) || {eldap_entry, Entry, _} <- Entries];
        _ -> ignore
    end,
    ok.

check_auth(_) ->
    Plain = #{client_id => <<"plain">>, username => <<"plain">>},
    Md5 = #{client_id => <<"md5">>, username => <<"md5">>},
    Sha = #{client_id => <<"sha">>, username => <<"sha">>},
    Sha256 = #{client_id => <<"sha256">>, username => <<"sha256">>},

    ok = emqx_access_control:authenticate(Plain, <<"plain">>),

    ok  = emqx_access_control:authenticate(Md5, <<"md5">>),

    ok = emqx_access_control:authenticate(Sha, <<"sha">>),

    ok = emqx_access_control:authenticate(Sha256, <<"sha256">>).

list_auth(_Config) ->
    application:start(emqx_auth_username),
    emqx_auth_username:add_user(<<"user1">>, <<"password1">>),
    User1 = #{client_id => <<"client1">>, username => <<"user1">>},
    ok = emqx_access_control:authenticate(User1, <<"password1">>),
    Plain = #{client_id => <<"client1">>, username => <<"plain">>},
    ok = emqx_access_control:authenticate(Plain, <<"plain">>),
    application:stop(emqx_auth_username).

comment_config(_) ->
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [auth_dn]],
    application:start(?APP),
    ?assertEqual([], emqx_access_control:lookup_mods(auth)).

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
    application:set_env(emqx, plugins_loaded_file, deps_path(emqx, "test/emqx_SUITE_data/loaded_plugins"));

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
