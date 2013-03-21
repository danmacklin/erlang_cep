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