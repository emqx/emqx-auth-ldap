%%--------------------------------------------------------------------
%% Copyright (c) 2012-2017 Feng Lee <feng@emqtt.io>.
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

-module(emq_auth_ldap_SUITE).

-compile(export_all).

-define(PID, emq_auth_ldap).

-define(APP, ?PID).

-define(AuthDN, "ou=test_auth,dc=emqtt,dc=com").

-include_lib("emqttd/include/emqttd.hrl").

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

all() -> 
    [{group, emq_auth_ldap_auth},
     {group, emq_auth_ldap}].

groups() ->
    [{emq_auth_ldap_auth, [sequence],
     [check_auth, list_auth]},
    {emq_auth_ldap, [sequence],
     [comment_config]}
    ].

init_per_suite(Config) ->
    [start_apps(App) || App <- [emqttd, emq_auth_ldap]],
    {ok, Handle} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    cleanup(Handle),
    prepare(Handle),
    Config.

end_per_suite(_Config) ->
    {ok, Handle} = ecpool_worker:client(gproc_pool:pick_worker({ecpool, ?PID})),
    cleanup(Handle),
    [application:stop(App) || App <- [emq_auth_ldap, emqttd]].

check_auth(_) ->
    Plain = #mqtt_client{client_id = <<"client1">>, username = <<"plain">>},
    Md5 = #mqtt_client{client_id = <<"md5">>, username = <<"md5">>},
    Sha = #mqtt_client{client_id = <<"sha">>, username = <<"sha">>},
    Sha256 = #mqtt_client{client_id = <<"sha256">>, username = <<"sha256">>},
    reload([{password_hash, plain}]),
    ok = emqttd_access_control:auth(Plain, <<"plain">>),
    reload([{password_hash, md5}]),
    ok  = emqttd_access_control:auth(Md5, <<"md5">>),
    reload([{password_hash, sha}]),
    ok = emqttd_access_control:auth(Sha, <<"sha">>),
    reload([{password_hash, sha256}]),
    ok = emqttd_access_control:auth(Sha256, <<"sha256">>).

list_auth(_Config) ->
    application:start(emq_auth_username),
    emq_auth_username:add_user(<<"user1">>, <<"password1">>),
    User1 = #mqtt_client{client_id = <<"client1">>, username = <<"user1">>},
    ok = emqttd_access_control:auth(User1, <<"password1">>),
    reload([{password_hash, plain}]),
    Plain = #mqtt_client{client_id = <<"client1">>, username = <<"plain">>},
    ok = emqttd_access_control:auth(Plain, <<"plain">>),
    application:stop(emq_auth_username).

comment_config(_) ->
    application:stop(?APP),
    [application:unset_env(?APP, Par) || Par <- [auth_dn]],
    application:start(?APP),
    ?assertEqual([], emqttd_access_control:lookup_mods(auth)).

reload(Config) when is_list(Config) ->
    application:stop(?APP),
    [application:set_env(?APP, K, V) || {K, V} <- Config],
    application:start(?APP).

start_apps(App) ->
    NewConfig = generate_config(App),
    lists:foreach(fun set_app_env/1, NewConfig).

generate_config(emqttd) ->
    Schema = cuttlefish_schema:files([local_path(["deps", "emqttd", "priv", "emq.schema"])]),
    Conf = conf_parse:file([local_path(["deps", "emqttd", "etc", "emq.conf"])]),
    cuttlefish_generator:map(Schema, Conf);

generate_config(emq_auth_ldap) ->
    Schema = cuttlefish_schema:files([local_path(["priv", "emq_auth_ldap.schema"])]),
    Conf = conf_parse:file([local_path(["etc", "emq_auth_ldap.conf"])]),
    cuttlefish_generator:map(Schema, Conf).


get_base_dir(Module) ->
    {file, Here} = code:is_loaded(Module),
    filename:dirname(filename:dirname(Here)).

get_base_dir() ->
    get_base_dir(?MODULE).

local_path(Components, Module) ->
    filename:join([get_base_dir(Module) | Components]).

local_path(Components) ->
    local_path(Components, ?MODULE).

set_app_env({App, Lists}) ->
    F = fun ({acl_file, _Var}) ->
                application:set_env(App, acl_file, local_path(["deps", "emqttd", "etc", "acl.conf"]));
            ({auth_dn, _Var}) ->
                application:set_env(App, auth_dn, "cn=%u,ou=test_auth,dc=emqtt,dc=com");
            ({Par, Var}) ->
                application:set_env(App, Par, Var)
        end,
    lists:foreach(F, Lists),
    application:ensure_all_started(App).

prepare(Handle) ->
    eldap:add(Handle, "dc=emqtt,dc=com",
                      [{"objectclass", ["dcObject", "organization"]},
                       {"dc", ["emqtt"]}, {"o", ["emqtt,Inc."]}]),
    
    ok = eldap:add(Handle, ?AuthDN,
                      [{"objectclass", ["organizationalUnit"]},
                       {"ou", ["test_auth"]}]),
    %% Add
    ok = eldap:add(Handle, "cn=plain," ++ ?AuthDN,
                 [{"objectclass", ["mqttUser"]},
                  {"cn", ["plain"]}, 
                  {"username", ["plain"]}, 
                  {"password", ["plain"]}]),

    ok = eldap:add(Handle, "cn=md5," ++ ?AuthDN,
                 [{"objectclass", ["mqttUser"]},
                  {"cn", ["md5"]}, {"username", ["md5"]}, {"password", ["1bc29b36f623ba82aaf6724fd3b16718"]}]),
    ok = eldap:add(Handle, "cn=sha," ++ ?AuthDN,
                 [{"objectclass", ["mqttUser"]},
                  {"cn", ["sha"]}, {"username", ["sha"]}, {"password", ["d8f4590320e1343a915b6394170650a8f35d6926"]}]),
    ok = eldap:add(Handle, "cn=sha256," ++ ?AuthDN,
                 [{"objectclass", ["mqttUser"]},
                  {"cn", ["sha256"]}, {"username", ["sha256"]}, {"password", ["5d5b09f6dcb2d53a5fffc60c4ac0d55fabdf556069d6631545f42aa6e3500f2e"]}]).

cleanup(Handle) ->
    case eldap:search(Handle, [{base, ?AuthDN},
                               {filter, eldap:present("objectclass")},
                               {scope,  eldap:wholeSubtree()}])
    of
        {ok, {eldap_search_result, Entries, _}} ->
            [ok = eldap:delete(Handle, Entry) || {eldap_entry, Entry, _} <- Entries];
        _ -> ignore
   end,
   ok.
