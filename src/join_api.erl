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

-module(join_api).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% Exported Functions
%%
-export([run_joins/2, create_echo_row_function/0, create_echo_join_function/0]).

%%
%% API Functions
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Joins = a list of {FeedName, WindowName, Query} 
%% 		Where a query is a list of {FeedName, WindowName, ['_', '_', 10], dataType} where datatype could be hardCoded, json or something else I haven't written yet!
%% 		or {FeedName, WindowName, ['_', '_', ], dataType}
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_joins([], _Row) ->
	[];

%% If the result of the join is empty then return an empty list.
run_joins(Joins, Row) ->
	lists:foldl(fun(Join, Acc) -> evaluate_join(Join, Row) ++ Acc end, [], Joins).

evaluate_join([FeedName, WindowName, Search, DataType], Row) ->
	SearchResults = do_join(WindowName, Search, Row, DataType),	
	produce_results(FeedName, WindowName, SearchResults).

do_join(WindowName, SearchParameter, _Row, hardCoded) ->
	search_api:do_search(WindowName, SearchParameter);

%% Perform a Search that requires json substitution = ["Obj.symbol", "Obj.volume", "Obj.price"]
do_join(WindowName, SearchParameter, Row, json) ->
	search_api:do_search(WindowName, search_api:perform_substitution(Row, SearchParameter)).

produce_results(_FeedName, _WindowName, []) ->
	[];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Results should be a list where the first element is the FeedName, the second the WindowName and the third a List
%% 		containing all of the results.
%% @end 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
produce_results(FeedName, WindowName, Results) ->
	[[FeedName, WindowName, Results]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_echo_row_function() ->
	<<"var rowFunction = function(parameters, joins, row, otherRow, sequence, matchRecogniseFirst){return [row]}">>.

create_echo_join_function() ->
		<<"var rowFunction = function(parameters, joins, row, otherRow, sequence, matchRecogniseFirst){return [joins]}">>.

-ifdef(TEST).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc This test sets up a feed with with two windows.  The first window is set
%%		up so that anything added to the feed is added to the window.  The second window 
%%		echos what has been added to it's join parameter. 
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
standard_end_to_end_3_elements_with_search_test() ->
	erlang_js:start(),
	
	application:start(erlang_cep),
	
	RowFunctionWindow1 = join_api:create_echo_row_function(),
	RowFunctionWindow2 = join_api:create_echo_join_function(),
	
	ReduceFunction = <<"">>,	
	QueryParameterList = [{numberOfMatches, 5}, {windowSize, 5}],
	
	%% set-up a search that retuns every element in the other window.
	Search = [[joinFeedOne, joinWinOne, ['_'], hardCoded]],
	
	feed_api:start_feed(joinFeedOne),
	
	feed_api:start_window(joinWinOne, joinFeedOne, RowFunctionWindow1, ReduceFunction, QueryParameterList, []),
	feed_api:start_window(joinWinTwo, joinFeedOne, RowFunctionWindow2, ReduceFunction, QueryParameterList, []),
	
	feed_api:add_searches(joinFeedOne, joinWinTwo, Search),
	
	lists:foreach(fun(Element) -> 
					feed_api:add_data(joinFeedOne, Element) 
				  end, lists:seq(0,3)),
	
	%% Query the join2win to output all elements

	Result = search_api:search_window(joinFeedOne, joinWinTwo, ['_'], ok, hardCoded),
	
	?assertEqual([[[[<<"joinFeedOne">>,<<"joinWinOne">>,
                                [[0],[2],[1]]]]],
                             [[[<<"joinFeedOne">>,<<"joinWinOne">>,
                                [[0],[1]]]]],
                             [[[<<"joinFeedOne">>,<<"joinWinOne">>,
                                [[0]]]]]], Result).

%% join_test_interactive() ->
%% 	RowFunctionWindow1 = join_api:create_echo_row_function(),
%% 	RowFunctionWindow2 = join_api:create_echo_join_function(),
%% 	ReduceFunction = <<"">>,
%% 	QueryParameterList = [{numberOfMatches, 5}, {windowSize, 5}],
%% 	Search = [[joinFeedOne, joinWinOne, ['_'], hardCoded]],
%% 	feed_api:start_feed(joinFeedOne),
%% 	feed_api:start_window(joinWinOne, joinFeedOne, RowFunctionWindow1, ReduceFunction, QueryParameterList, []),
%% 	feed_api:start_window(joinWinTwo, joinFeedOne, RowFunctionWindow2, ReduceFunction, QueryParameterList, []),
%% 	feed_api:add_searches(joinFeedOne, joinWinTwo, Search),
%% 	
%% 	ok = timer:sleep(1000),
%% 	
%% 	io:format("Searches Added = ~p ~n", [feed_api:view_searches(joinFeedOne, joinWinTwo)]),
%% 
%% 	lists:foreach(fun(Element) -> feed_api:add_data(joinFeedOne, Element), ok = timer:sleep(100) end, lists:seq(0,3)),
%% 
%% 	search_api:search_window(joinFeedOne, joinWinTwo, ['_'], ok, hardCoded).

-endif.