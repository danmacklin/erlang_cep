
-module(erlang_cep_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
	%%Global ETS window pid registry
	ets:new(window_ets, [named_table, public, {read_concurrency,true}, {keypos,1}]),
	%%ets:new(feed_ets, [named_table, public, {read_concurrency,true}, {keypos,1}]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, []} }.