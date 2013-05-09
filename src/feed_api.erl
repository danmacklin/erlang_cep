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

-module(feed_api).

%%
%% Include files
%%

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("cep_logger.hrl").

%%
%% Exported Functions
%%
-export([start_feed/1, get_feed_genserver_name/1, crash_feed_genserver/1, start_window/6, stop_window/2,
		 add_data/2, do_add_data/3, subscribe_feed_window/3, do_subscribe_feed_window/2, receive_message/1,
		 add_searches/3, view_searches/2, remove_searches/3, do_add_searches/3, do_view_searches/2, 
		 do_remove_searches/3]).

%%
%% API Functions
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Starts a supervision hierarchy for a new feed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_feed(FeedName) ->
	{ok, Pid} = feed_sup:start_link(FeedName),
	Pid.

get_feed_genserver_name(FeedName) ->
	list_to_atom(string:concat(atom_to_list(FeedName), "Genserver")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Used to test the supervisor hierarchy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crash_feed_genserver(FeedName) ->
	gen_server:cast(get_feed_genserver_name(FeedName), {generateError}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Start a new window for this feed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_window(WindowName, FeedName, RowFunction, ReduceFunction, QueryParameterList, Parameters) ->
	QueryParameters = query_parameter_api:get_parameters(QueryParameterList),
	gen_server:cast(get_feed_genserver_name(FeedName), {startWindow, WindowName, FeedName, RowFunction, ReduceFunction, QueryParameters, Parameters}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Stop a window
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stop_window(FeedName, WindowName) ->
	gen_server:cast(get_feed_genserver_name(FeedName), {stopWindow, WindowName}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Subscribe to a feeds window
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subscribe_feed_window(FeedName, WindowName, Pid) ->
	gen_server:cast(get_feed_genserver_name(FeedName), {subscribe, WindowName, Pid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Called by feed_genserver to do the subscribe
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_subscribe_feed_window(WindowName, Pid) ->
	window_api:subscribe(WindowName, Pid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Add data to a feed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_data(FeedName, Data) ->
	gen_server:cast(get_feed_genserver_name(FeedName), {addData, Data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Called by feed_gen_server to send the data to each window
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do_add_data(Data, WindowPidsList, SearchDict) ->
	%% Send the data to each window
	lists:foreach(fun({WindowName, Pid}) ->
						  
						  %% Get any searches for this window and execute them then add to the data
						  Joins = join_api:run_joins(do_view_searches(SearchDict, WindowName), Data), 
						  ?DEBUG("do_add_data WindowName: ~p Joins: ~p", [WindowName, Joins]),
						  gen_server:cast(Pid, {add_data, Data, Joins})
				  end, WindowPidsList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Add a list of searches to a window within a feed
%%		Searches are in the format ({feedName, WindowName, [SearchParameters], SearchType}).
%%
%%		For example [testFeed, testWin, ['_', '_', <<"Goog">>], hardCoded]
%%		will create a search on the window testWin, on the feed testFeed, where the first two parameters
%%	  	are wild cards and the third is the String Goog.  
%%
%%		N.B. We are not performing any parameter substitution here (i.e pulling a parameter out of a new incoming row).
%%		
%%	    Or [testFeed, testWin, ["Obj.symbol", '_' , "Obj.price"], json]
%%		will create a search on window testWin, on the feed testFeed, where the first paramter is the object field of the
%%		incoming row, the second is a wildcard and the third the price field of the incoming row.  
%%
%%		Searches are used to perform joins between windows. So for example lets say that you had one window containing
%%		all of the stocks your client has bought in the last 10 minutes, and another window that looked for interesting
%%      trades.  You could use a search to check that the stock ticker name for a new row being added is not within the
%%		bought window.
%%	
%%		When you add a search to a window, it calls out to the window identified in the search.  The results of the search
%%		are then accessible via the joins parameter in your row function.
%%
%%		The joins are returned as a javascript array where the first element is the feedName, the second the windowName, 
%%		the third is a list of lists where the inner list holds a row of data returned in the search wrapped in the outer
%%		outer list i.e. a list of results per row.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_searches(FeedName, WindowName, Searches) ->
	gen_server:cast(get_feed_genserver_name(FeedName), {addSearches, WindowName, Searches}).

do_add_searches(SearchDict, WindowName, Searches) ->
	dict:append_list(WindowName, Searches, SearchDict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc View a list of the searches added to this window
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
view_searches(FeedName, WindowName) ->
	gen_server:call(get_feed_genserver_name(FeedName), {viewSearches, WindowName}).

do_view_searches(SearchDict, WindowName) ->
	case dict:find(WindowName, SearchDict) of
		error ->
			[];
		{ok, Value} ->
			?DEBUG("do_view_searches WindowName: ~p Search: ~p", [WindowName, Value]),
			Value
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Remove a list of searches from the window.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_searches(FeedName, WindowName, Searches) ->
	gen_server:cast(get_feed_genserver_name(FeedName), {removeSearches, WindowName, Searches}).

do_remove_searches(WindowName, Searches, SearchDict) ->
	NewSearchList = lists:foldl(fun(Element, Acc) ->
					lists:delete(Element, Acc)
				end, do_view_searches(SearchDict, WindowName), Searches),
	dict:store(WindowName, NewSearchList, SearchDict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

receive_message(Pid) ->
	receive
		Results ->
			timer:sleep(1000),
			Pid ! Results
	end.

-ifdef(TEST).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc An example showing how to use Erlang functions rather than javascript to
%%		perform a match recognise match.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
erlang_match_recognise_test() -> 
	Data = window_api:create_json([{"1.01", "14"}, {"1.02", "15"}, {"2.00", "16"}]),
	RowFunction = {example_erlang_match_recognise, match_recognise_row_function},
	ReduceFunction = {example_erlang_match_recognise, match_recognise_reduce_function},
	
	QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}, {matchType, matchRecognise}],
	
	feed_api:start_feed(erlangTestFeedMatchRecognise),
	
	feed_api:start_window(erlangTestWinFeedMatchRecognise, erlangTestFeedMatchRecognise, RowFunction, ReduceFunction,  QueryParameterList, [0.50]),
	
	Pid = spawn(?MODULE, receive_message, [self()]),
	
	subscribe_feed_window(erlangTestFeedMatchRecognise, erlangTestWinFeedMatchRecognise, Pid),
	
	lists:foreach(fun (DataElement) ->
					add_data(erlangTestFeedMatchRecognise, DataElement)
				  end, Data),
	receive
		Results ->
			?assertEqual(15.0, Results)
	after 4000 ->
			io:format("Timed out ~n"),
			p=y
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc This test sets up a feed with a standard window looking for the volume to 
%% 		increase each time.  It should fire the reduce function that will calculate
%% 		the average volume.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
standard_end_to_end_3_elements_with_search_test() ->
	erlang_js:start(),
	
	application:start(erlang_cep),
	
	Data = window_api:create_json([{"1.01", "10"}, {"1.02", "11"}, {"2.00", "12"}]),
	RowFunction = window_api:create_standard_row_function(),
	ReduceFunction = window_api:create_reduce_function(),
	
	QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}],
	
	start_feed(testFeed),
	
	start_window(testWinFeed, testFeed, RowFunction, ReduceFunction, QueryParameterList, [1.00]),
	
	Pid = spawn(?MODULE, receive_message, [self()]),
	
	subscribe_feed_window(testFeed, testWinFeed, Pid),
	
	lists:foreach(fun (DataElement) ->
					add_data(testFeed, DataElement)
				  end, Data),
	receive
		Results ->
			?assertEqual(11, Results)
	after 4000 ->
			io:format("Timed out ~n"),
			p=y
	end,
	
	?assertEqual([[<<"GOOG">>,2,12]], search_api:search_window(testFeed, testWinFeed, [<<"GOOG">>, '_' , 12], ok, hardCoded)),
	?assertEqual([[<<"GOOG">>,2,12]], 
				 search_api:search_window(testFeed, testWinFeed, ["Obj.symbol", '_' , "Obj.volume"], 
										  window_api:create_single_json("2.00", "12"), json)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc This test uses a matchRecognise function to check that the volume is increasing each time within the window.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
match_recognise_end_to_end_3_elements_test() ->	

	Data = window_api:create_json([{"1.01", "14"}, {"1.02", "15"}, {"2.00", "16"}]),
	RowFunction = window_api:create_match_recognise_row_function(),
	ReduceFunction = window_api:create_reduce_function(),
	
	QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}, {matchType, matchRecognise}],
	
	start_feed(testFeedMatchRecognise),
	
	start_window(testWinFeedMatchRecognise, testFeedMatchRecognise, RowFunction, ReduceFunction,  QueryParameterList, [0.50]),
	
	Pid = spawn(?MODULE, receive_message, [self()]),
	
	subscribe_feed_window(testFeedMatchRecognise, testWinFeedMatchRecognise, Pid),
	
	lists:foreach(fun (DataElement) ->
					add_data(testFeedMatchRecognise, DataElement)
				  end, Data),
	receive
		Results ->
			?assertEqual(15, Results)
	after 4000 ->
			io:format("Timed out ~n"),
			p=y
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc A timed window test
%%%%%%%%%%%%%%%%%%%%%%%%%%%
time_match_recognise_end_to_end_3_elements_test() ->	
	Data = window_api:create_json([{"1.01", "14"}, {"1.02", "15"}, {"2.00", "16"}]),
	RowFunction = window_api:create_match_recognise_row_function(),
	ReduceFunction = window_api:create_reduce_function(),
	
	QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}, {matchType, matchRecognise}, {windowType, time}],
	
	start_feed(time),
	
	start_window(timeWin, time, RowFunction, ReduceFunction, QueryParameterList, [0.50]),
	
	Pid = spawn(?MODULE, receive_message, [self()]),
	
	subscribe_feed_window(time, timeWin, Pid),
	
	lists:foreach(fun (DataElement) ->
					ok = timer:sleep(1000),
					add_data(time, DataElement)
				  end, Data),
	receive
		Results ->
			?assertEqual(15, Results)
	after 4000 ->
			io:format("Timed out ~n"),
			p=y
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc A timed every window test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
time_every_end_to_end_3_elements_test() ->	
	Data = window_api:create_json([{"1.01", "14"}, {"1.02", "15"}, {"2.00", "16"}]),
	RowFunction = window_api:create_match_recognise_row_function(),
	ReduceFunction = window_api:create_reduce_function(),
	
	QueryParameterList = [{windowSize, 2}, {matchType, every}, {windowType, time}],
	
	start_feed(every),
	
	start_window(everyWin, every, RowFunction, ReduceFunction, QueryParameterList, [0.50]),
	
	Pid = spawn(?MODULE, receive_message, [self()]),
	
	subscribe_feed_window(every, everyWin, Pid),
	
	lists:foreach(fun (DataElement) ->
					add_data(every, DataElement)
				  end, Data),
	receive
		Results ->
			io:format("fired~n"),
			?assertEqual(15, Results)
	after 4000 ->
			io:format("Timed out ~n"),
			p=y
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Test setting up a window adding some searches
%%		then removing them.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
setup_search_test() ->
	QueryParameterList = [{windowSize, 2}, {matchType, every}, {windowType, size}],
 	start_feed(setup_search_feed),
	start_window(setup_search_feedWin, setup_search_feed, <<"">>, <<"">>, QueryParameterList, []),
	
	Searches = [[setup_search_feed, setup_search_feedWin, ['_', '_', <<"Goog">>], hardCoded], [setup_search_feed, setup_search_feedWin, ['_', '_', <<"Goog">>], json]],
	
	feed_api:add_searches(setup_search_feed, setup_search_feedWin, Searches),
	
	?assertEqual(Searches, feed_api:view_searches(setup_search_feed, setup_search_feedWin)),
	
	feed_api:remove_searches(setup_search_feed, setup_search_feedWin, [[setup_search_feed, setup_search_feedWin, ['_', '_', <<"Goog">>], hardCoded]]),
	?assertEqual([[setup_search_feed, setup_search_feedWin, ['_', '_', <<"Goog">>], json]], feed_api:view_searches(setup_search_feed, setup_search_feedWin)).

-endif.