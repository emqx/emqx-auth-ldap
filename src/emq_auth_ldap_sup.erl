%%
%% Copyright (c) 2013-2017 EMQ Enterprise, Inc. All Rights Reserved.
%%

-module(emq_auth_ldap_sup).

-behaviour(supervisor).

-include("emq_auth_ldap.hrl").

-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    %% ldap Connection Pool.
    {ok, Server} = application:get_env(?APP, ldap),
    PoolSpec = ecpool:pool_spec(?APP, ?APP, emq_auth_ldap_cli, Server),
    {ok, {{one_for_one, 10, 100}, [PoolSpec]}}.

