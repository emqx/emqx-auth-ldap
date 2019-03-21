%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-import(proplists, [get_value/2]).

-import(emqx_auth_ldap_cli, [search/2, init_args/1]).

-export([check/2, description/0]).

-define(UNDEFINED(Username), (Username =:= undefined orelse Username =:= <<>>)).

check(#{username := Username}, _State) 
  when ?UNDEFINED(Username) ->
    {ok, #{auth_result => bad_username_or_password}};

check(Credentials = #{username := Username, password := Password},
      State = #{password_attr := PasswdAttr}) ->
    case lookup_user(Username, State) of
        undefined -> {ok, Credentials};
        {error, Error} -> {error, Error};
        Attributes ->
            case get_value(PasswdAttr, Attributes) of
                undefined ->
                    logger:error("LDAP Search State: ~p, uid: ~p, result:~p", [State, Username, Attributes]),
                    {ok, Credentials};
                [Passhash1] ->
                    format_password(Passhash1, Password, Credentials)
            end
    end.

lookup_user(Username, #{username_attr := UidAttr,
                        match_objectclass := ObjectClass,
                        device_dn := DeviceDn}) ->
    Filter = eldap2:equalityMatch("objectClass", ObjectClass),
    case search(lists:concat([UidAttr,"=", binary_to_list(Username), ",", DeviceDn]), Filter) of
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

check_pass(Password, Password, Credentials) -> {ok, Credentials#{auth_result => success}};
check_pass(_, _, _) -> {error, password_error}.

format_password(Passhash, Password, Credentials) ->
    case do_format_password(Passhash, Password) of
        {error, Error2} ->
            {error, Error2};
        {Passhash1, Password1} ->
            check_pass(Passhash1, Password1, Credentials)
    end.

do_format_password(Passhash, Password) ->
    Base64PasshashHandler = handle_passhash(fun(HashType, Passhash1, Password1) ->
                                                Passhash2 = binary_to_list(base64:decode(Passhash1)),
                                                resolve_passhash(HashType, Passhash2, Password1) 
                                            end,
                                            fun(_Passhash, _Password) -> 
                                                {error, password_error} 
                                            end),
    PasshashHandler = handle_passhash(fun resolve_passhash/3,
                                      Base64PasshashHandler),
    PasshashHandler(Passhash, Password).

resolve_passhash(HashType, Passhash, Password) ->
    [_, Passhash1] = string:tokens(Passhash, "}"),
    do_resolve(HashType, Passhash1, Password).

handle_passhash(HandleMatch, HandleNoMatch) ->
    fun(Passhash, Password) ->
            case re:run(Passhash, "(?<={)[^{}]+(?=})", [{capture, all, list}, global]) of
                {match, [[HashType]]} ->
                    io:format("~n Passhash:~p HashType:~p ~n", [Passhash, HashType]),
                    HandleMatch(list_to_atom(string:to_lower(HashType)), Passhash, Password);
                _ ->
                    HandleNoMatch(Passhash, Password)
            end
    end.

do_resolve(ssha, Passhash, Password) ->
    D64 = base64:decode(Passhash),
    {HashedData, Salt} = lists:split(20, binary_to_list(D64)),
    NewHash = crypto:hash(sha, <<Password/binary, (list_to_binary(Salt))/binary>>),
    {list_to_binary(HashedData), NewHash};
do_resolve(HashType, Passhash, Password) ->
    Password1 = base64:encode(crypto:hash(HashType, Password)),
    {list_to_binary(Passhash), Password1}.

description() -> "LDAP Authentication Plugin".
