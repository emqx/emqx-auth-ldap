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

-define(AUTHDN, "ou=test_auth,dc=emqx,dc=io").

-define(TESTAUTHDN, "cn=%u,ou=test_auth,dc=emqx,dc=io").

-define(ACLDN, "ou=test_acl,dc=emqx,dc=io").

-define(TESTACLDN, "ou=test_acl,dc=emqx,dc=io").

-include_lib("emqx/include/emqx.hrl").

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, emqx_auth_ldap_auth},
     {group, emqx_auth_ldap}].

groups() ->
    [{emqx_auth_ldap_auth, [sequence], [check_auth, list_auth]},
     {emqx_auth_ldap, [sequence], [comment_config]}].

init_per_suite(Config) ->
    [start_apps(App, SchemaFile, ConfigFile) ||
        {App, SchemaFile, ConfigFile}
            <- [{emqx, deps_path(emqx, "priv/emqx.schema"),
                       deps_path(emqx, "etc/emqx.conf")},
                {emqx_auth_ldap, local_path("priv/emqx_auth_ldap.schema"),
                                 local_path("etc/emqx_auth_ldap.conf")}]],
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(emqx_access_control, authenticate, x),
    dbg:tpl(emqx_auth_ldap, check, x),
    dbg:tpl(emqx_auth_ldap_cli, gen_filter, x),
    dbg:tpl(emqx_auth_ldap_cli, fill, x),
    dbg:tpl(eldap, search, x),
    prepare(),
    Config.

end_per_suite(_Config) ->
    clean(),
    [application:stop(App) || App <- [emqx_auth_ldap, emqx]].

prepare() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    init_acl(Pid),
    init_auth(Pid).

clean() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    init_acl(Pid),
    init_auth(Pid).

init_acl(_Pid) ->
    ok.

clean_acl() ->
    {ok, _Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})).

init_auth(Pid) ->
    eldap:add(Pid, "dc=emqx,dc=io",
                   [{"objectclass", ["dcObject", "organization"]},
                    {"dc", ["emqx"]}, {"o", ["emqx,Inc."]}]),
    eldap:add(Pid, ?AUTHDN,
                   [{"objectclass", ["organizationalUnit"]},
                    {"ou", ["test_auth"]}]),
    eldap:add(Pid, "cn=plain," ++ ?AUTHDN,
                   [{"objectclass", ["mqttUser"]},
                    {"cn", ["plain"]},
                    {"username", ["plain"]},
                    {"password", ["plain"]}]),
    eldap:add(Pid, "cn=md5," ++ ?AUTHDN,
                   [{"objectclass", ["mqttUser"]},
                    {"cn", ["md5"]}, {"username", ["md5"]}, {"password", ["1bc29b36f623ba82aaf6724fd3b16718"]}]),
    eldap:add(Pid, "cn=sha," ++ ?AUTHDN,
                   [{"objectclass", ["mqttUser"]},
                    {"cn", ["sha"]}, {"username", ["sha"]}, {"password", ["d8f4590320e1343a915b6394170650a8f35d6926"]}]),
    eldap:add(Pid, "cn=sha256," ++ ?AUTHDN,
                   [{"objectclass", ["mqttUser"]},
                    {"cn", ["sha256"]}, {"username", ["sha256"]}, {"password", ["5d5b09f6dcb2d53a5fffc60c4ac0d55fabdf556069d6631545f42aa6e3500f2e"]}]).

clean_auth() ->
    {ok, Pid} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    case eldap:search(Pid, [{base, ?AUTHDN},
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
    reload([{password_hash, plain}]),
    ok = emqx_access_control:authenticate(Plain, <<"plain">>),
    reload([{password_hash, md5}]),
    ok  = emqx_access_control:authenticate(Md5, <<"md5">>),
    reload([{password_hash, sha}]),
    ok = emqx_access_control:authenticate(Sha, <<"sha">>),
    reload([{password_hash, sha256}]),
    ok = emqx_access_control:authenticate(Sha256, <<"sha256">>).

list_auth(_Config) ->
    application:start(emqx_auth_username),
    emqx_auth_username:add_user(<<"user1">>, <<"password1">>),
    User1 = #{client_id => <<"client1">>, username => <<"user1">>},
    ok = emqx_access_control:authenticate(User1, <<"password1">>),
    reload([{password_hash, plain}]),
    Plain = #{client_id => <<"client1">>, username => <<"plain">>},
    ok = emqx_access_control:authenticate(Plain, <<"plain">>),
    application:stop(emqx_auth_username).

comment_config(_) ->
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [auth_dn]],
    application:start(?APP),
    ?assertEqual([], emqx_access_control:lookup_mods(auth)).

reload(Config) when is_list(Config) ->
    application:stop(?APP),
    [application:set_env(?APP, K, V) || {K, V} <- Config],
    application:start(?APP).

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
set_special_configs(emqx_auth_ldap) ->
    application:set_env(emqx_auth_ldap, auth_dn, ?TESTAUTHDN),
    application:set_env(emqx_auth_ldap, acl_dn, ?TESTACLDN);
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
