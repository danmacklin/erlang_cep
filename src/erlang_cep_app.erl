-module(erlang_cep_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("Application starting ~n", []),
    erlang_cep_sup:start_link().

stop(_State) ->
    ok.
