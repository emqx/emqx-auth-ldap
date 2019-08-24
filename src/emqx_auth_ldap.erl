%%--------------------------------------------------------------------
%% Copyright (c) 2019 EMQ Technologies Co., Ltd. All Rights Reserved.
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

-module(emqx_auth_ldap).

-include_lib("emqx/include/emqx.hrl").
-include_lib("eldap/include/eldap.hrl").
-include_lib("emqx/include/logger.hrl").

-import(proplists, [get_value/2]).

-import(emqx_auth_ldap_cli, [search/2]).

-export([ register_metrics/0
        , check/3
        , description/0
        ]).

-define(AUTH_METRICS,
        ['auth.ldap.success',
         'auth.ldap.failure',
         'auth.ldap.ignore'
        ]).

register_metrics() ->
    [emqx_metrics:new(MetricName) || MetricName <- ?AUTH_METRICS].

check(Client= #{username := Username, password := Password}, AuthResult,
      State = #{password_attr := PasswdAttr}) ->
    CheckResult = case lookup_user(Username, State) of
                      undefined -> {error, not_found};
                      {error, Error} -> {error, Error};
                      Attributes ->
                          case get_value(PasswdAttr, Attributes) of
                              undefined ->
                                  logger:error("LDAP Search State: ~p, uid: ~p, result:~p",
                                               [State, Username, Attributes]),
                                  {error, not_found};
                              [Passhash1] ->
                                  format_password(Passhash1, Password, Client)
                          end
                  end,
    case CheckResult of
        ok ->
            emqx_metrics:inc('auth.ldap.success'),
            {stop, AuthResult#{auth_result => success, anonymous => false}};
        {error, not_found} ->
            emqx_metrics:inc('auth.ldap.ignore'), ok;
        {error, ResultCode} ->
            ?LOG(error, "[LDAP] Auth from ldap failed: ~p", [ResultCode]),
            emqx_metrics:inc('auth.ldap.failure'),
            {stop, AuthResult#{auth_result => ResultCode, anonymous => false}}
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
            ?LOG(error, "[LDAP] Search dn: ~p, filter: ~p, fail:~p", [DeviceDn, Filter, Error]),
            {error, username_or_password_error}
    end.

check_pass(Password, Password, _Client) -> ok;
check_pass(_, _, _) -> {error, bad_username_or_password}.

format_password(Passhash, Password, Client) ->
    case do_format_password(Passhash, Password) of
        {error, Error2} ->
            {error, Error2};
        {Passhash1, Password1} ->
            check_pass(Passhash1, Password1, Client)
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

