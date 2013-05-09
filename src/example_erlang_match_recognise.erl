%% -------------------------------------------------------------------
%%
%% erlang_cep:
%%
%% Copyright (c) 2013 Daniel Macklin.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(example_erlang_match_recognise).

%%
%% Include files
%%

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% Exported Functions
%%

-export([match_recognise_row_function/6, match_recognise_reduce_function/1]).

match_recognise_row_function(Parameters, _Joins, Row, _OtherRow, _Sequence, true) ->
	ParsedJson = mochijson2:decode(Row),
	Symbol = parse_json:destructure("Obj.symbol", ParsedJson),
	Price = parse_json:destructure("Obj.price", ParsedJson),
	Volume = parse_json:destructure("Obj.volume", ParsedJson),
	
	case price > lists:nth(1, Parameters) of
		true ->
			[Symbol, Price, Volume];
		false ->
			[]
	end;
	
match_recognise_row_function(_Parameters, _Joins, Row, OtherRow, _Sequence, false) ->
	ParsedJson = mochijson2:decode(Row),
	OtherRowParsedJson = mochijson2:decode(OtherRow),
	Volume = parse_json:destructure("Obj.volume", ParsedJson),
	
	OtherRowVolume = parse_json:destructure("Obj.volume", OtherRowParsedJson),
	
	case Volume > OtherRowVolume of
		true ->
			true;
		false ->
			false
	end.

match_recognise_reduce_function(Matches) ->
	TotalSum = lists:foldr(fun(Value, Sum) -> Sum + lists:nth(3, Value) end, 0, Matches),
	TotalSum / length(Matches).