-module(parse_json).
-export([destructure/2]).

-include_lib("eunit/include/eunit.hrl").

destructure(JS, JSON) ->
    F = json:parse(JS),
    F(JSON).

parse_json_test() ->
	Data =  window_api:create_single_json("10.00","10"),
	Obj =  mochijson2:decode(Data),
	?assertEqual(10,parse_json:destructure("Obj.volume", Obj)).