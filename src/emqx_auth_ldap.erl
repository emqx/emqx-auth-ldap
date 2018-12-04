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

-module(emqx_auth_ldap).

-include_lib("emqx/include/emqx.hrl").
-include_lib("eldap/include/eldap.hrl").

-behaviour(emqx_auth_mod).

-import(emqx_auth_ldap_cli, [search/2]).

-import(proplists, [get_value/2, get_value/3]).

-export([init/1, check/3, description/0]).

-define(UNDEFINED(Username), (Username =:= undefined orelse Username =:= <<>>)).

init(ENVS) ->
    DeviceDn = get_value(device_dn, ENVS, "ou=device,ou=Auth,ou=MQ,dc=emqx,dc=io"),
    ObjectClass = get_value(objectclass, ENVS, "mqttUser"),
    {ok, #{device_dn => DeviceDn,
           objectclass => ObjectClass}}.

check(#{username := Username}, _Password, _State) when ?UNDEFINED(Username) ->
    {error, username_undefined};

check(#{username := Username}, Password, #{device_dn   := DeviceDn,
                                           objectclass := ObjectClass}) ->
    case lookup_user(DeviceDn, ObjectClass, Username) of
        undefined -> ignore;
        {error, Error} -> {error, Error};
        Attributes ->
            AttributesHandler = handle_attributes(Attributes, Password),
            AttributesHandler(Attributes, Password)
    end.
    
handle_attributes(Attributes, Password) ->
    fun(DeviceDn, Username) ->
        logger:error("LDAP Search dn: ~p, uid: ~p, result:~p", [DeviceDn, Username, Attributes]),
        case get_value("userPassword", Attributes) of
            undefined ->
                ok;
            [Passhash1] ->
                do_format_password(Passhash1, Password)
        end
    end.

lookup_user(DeviceDn, ObjectClass, Uid) ->
    Filter = eldap2:equalityMatch("objectClass", ObjectClass),
    case search(lists:concat(["uid=", binary_to_list(Uid), ",", DeviceDn]), Filter) of
        {error, noSuchObject} ->
            undefined;
        {ok, #eldap_search_result{entries = [Entry]}} ->
            Attributes = Entry#eldap_entry.attributes,
            case get_value("isEnabled", Attributes) of
                undefined ->
                    Attributes;
                [Val] ->
                    case list_to_atom(string:to_lower(Val)) of
                        true -> Attributes;
                        false -> {error, username_disabled}
                    end
            end;
        {error, Error} ->
            logger:error("LDAP Search dn: ~p, filter: ~p, fail:~p", [DeviceDn, Filter, Error]),
            {error, username_or_password_error}
    end.

check_pass(Passhash, Password) when Passhash =:= Password -> ok;
check_pass(_, _) -> {error, password_error}.

do_format_password(Passhash, Password) ->
    case format_password(Passhash, Password) of
        {error, Error2} ->
            {error, Error2};

        {Passhash1, Password1} ->
            check_pass(Passhash1, Password1)
    end.

format_password(Passhash, Password) ->
    Base64PasshashHandler = handle_passhash(fun(HashType, Passhash1, Password1) ->
                                                Passhash2 = binary_to_list(base64:decode(Passhash1)),
                                                resolve_passhash(HashType, Passhash2, Password1) 
                                            end,
                                            fun() -> {error, password_error} end),
    PasshashHandler = handle_passhash(fun resolve_passhash/3,
                                      Base64PasshashHandler),
    PasshashHandler(Passhash, Password).

resolve_passhash(HashType, Passhash, Password) ->
    [_, Passhash1] = string:tokens(Passhash, "}"),
    Password1 = base64:encode(crypto:hash(list_to_atom(string:to_lower(HashType)), Password)),
    {Passhash1, binary_to_list(Password1)}.

handle_passhash(HandleMatch, HandleNoMatch) ->
    fun(Passhash, Password) ->
            case re:run(Passhash, "(?<={)[^{}]+(?=})", [{capture, all, list}, global]) of
                {match, [[HashType]]} ->
                    HandleMatch(HashType, Passhash, Password);
                _ ->
                    HandleNoMatch()
            end
    end.

description() -> "LDAP Authentication Plugin".
