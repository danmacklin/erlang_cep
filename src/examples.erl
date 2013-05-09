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

-module(examples).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A few examples to get you used to using this system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Exported Functions
%%
-export([every_window/0, create_single_sales_json/3, receive_message/2, join_window/0, create_join_window_dan_match_function/0, 
		 create_join_window_match_function/0, receive_pattern_message/0, create_pattern_row_function/0, create_pattern_reduce_function/0,
		 pattern_window/0]).
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Stuff needed to set-up examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_some_data() ->
	List = [{"STK1", "1.01", "10"},
	 		{"STK1", "1.01", "10"},
	 		{"STK1", "1.01", "10"},
	 		{"STK1", "1.01", "10"},
	 		{"STK1", "1.01", "10"},
	 		{"STK1", "1.01", "10"},
	 		{"STK1", "1.01", "10"},
	 		{"STK1", "1.01", "10"},
	 		{"STK1", "1.01", "10"},
	 		{"STK1", "1.01", "10"}],
	
	create_json(List).

create_json(Data) ->
	lists:foldl(fun({StockCode, UnitSalePrice, Volume}, Acc) -> 
					[create_single_sales_json(StockCode, UnitSalePrice, Volume) | Acc]
				end, [], lists:reverse(Data)).

create_single_sales_json(StockCode, UnitSalePrice, Volume) ->
	list_to_binary(["{\"stockCode\": \"", StockCode, "\",""\"unitSalePrice\": ", UnitSalePrice , ",\"volume\": ",  Volume,"}"]).

receive_message(Pid, No) when No =< 10 ->
	receive
		Results ->
			io:format("Receive ~p ~n",[Results]),
			timer:sleep(1000),
			Pid ! Results,
			receive_message(Pid, No + 1)
	end;
	
receive_message(_Pid, _No) ->
							 ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Set-up a feed with a five second window, performing
%% 		an every match.
%%
%% 		A typical use case for this kind of window would
%% 		be to store sales data, and then average it up
%% 		every to seconds.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
every_window() ->
	RowFunction = create_every_window_match_function(),
	ReduceFunction = create_every_reduce_function(),
	
	Data = generate_some_data(),
	
	QueryParameterList = [{windowSize, 5}, {matchType, every}, {windowType, time}],
	
	feed_api:start_feed(everyFeed),
	
	feed_api:start_window(everyFeedWin, everyFeed, RowFunction, ReduceFunction, QueryParameterList, []),
	
	Pid = spawn(?MODULE, receive_message, [self(), 0]),
	
	feed_api:subscribe_feed_window(everyFeed, everyFeedWin, Pid),
	
	lists:foreach(fun (DataElement) ->
					feed_api:add_data(everyFeed, DataElement)
				  end, Data),
	receive
		Results ->
			io:format("And the answer is ~p ~n", [Results])
	after 10000 ->
			io:format("Timed out ~n")
	end,
	
	lists:foreach(fun (DataElement) ->
					feed_api:add_data(everyFeed, DataElement)
				  end, Data),
	receive
		Results2 ->
			io:format("And the answer2 is ~p ~n", [Results2])
	after 10000 ->
			io:format("Timed out ~n")
	end.

create_every_window_match_function() ->
	<<"var rowFunction = function(parameters, joins, row, otherRow, sequence, matchRecogniseFirst){
							var myObject = JSON.parse(row);
							stockCode = myObject.stockCode;
							unitSalePrice = myObject.unitSalePrice;
							volume = myObject.volume;

							return [stockCode, unitSalePrice, volume]}">>.

create_every_reduce_function() ->
	<<"var reduceFunction = function(matches){
							var sumUnitSalePrice = 0;
							var sumVolume = 0;

							for(var i=0; i<matches.length; i++) {
								var match = matches[i];
								sumUnitSalePrice += match[1];
								sumVolume += match[2];
							}

							var averageUnitSalePrice = 0;
							var averageVolume = 0;

							if (sumUnitSalePrice > 0){
								averageUnitSalePrice = sumUnitSalePrice / matches.length;
							}

							if (sumVolume > 0){
								averageVolume = sumVolume / matches.length;
							}

							return [averageUnitSalePrice, averageVolume]}">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc A join example Create one size based window with the value "dan" stored within it.
%% 		Create a second window that will add the value join if the value "dan" is found
%% 		within the first window.  Note that this example does not need a reduce function
%%		normally this would not be the case.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_join_window_dan_match_function() ->
	<<"var rowFunction = function(parameters, joins, row, otherRow, sequence, matchRecogniseFirst){ if(sequence == 0){}; return [ \"Dan\" ]}">>.

create_join_window_match_function() ->
	<<"var rowFunction = function(parameters, joins, row, otherRow, sequence){

							var j = new Join(joins);

							ejsLog(\"/tmp/foo.txt\", j.exists(parameters[0], 0));

							if (j.exists(parameters[0], 0)){
								return [\"Join\"];
							}
		
							return [\"Nope\"]}
	">>.

%% This is what you call to run the example
join_window() ->
	QueryParameterList = [{numberOfMatches, 4}, {windowSize, 4}],
	
	feed_api:start_feed(jFeed),
	feed_api:start_feed(jFeed2),
	feed_api:start_feed(jFeed3),
	
	feed_api:start_window(jf1Win, jFeed, examples:create_join_window_dan_match_function(), <<"">>, QueryParameterList, []),
	feed_api:start_window(jf2Win, jFeed2, examples:create_join_window_dan_match_function(), <<"">>, QueryParameterList, []),
	feed_api:start_window(jf3Win, jFeed3, examples:create_join_window_match_function(), <<"">>, QueryParameterList, [<<"Dan">>]),
	
	Search = [[jFeed, jf1Win, [<<"Dan">>], hardCoded],[jFeed2, jf2Win, [<<"Dan">>], hardCoded]],
	
	feed_api:add_searches(jFeed3,jf3Win,Search),
	
	%% Add some data
	feed_api:add_data(jFeed, "d"),
	feed_api:add_data(jFeed2, "d"),
	feed_api:add_data(jFeed3, "d"),

	%% Now do a search over jfWin3 answer should be <<"Join">>  as Dan should be found in jf1Win and jf2Win
	io:format("Search results = ~p ~n", [search_api:search_window(jFeed3, jf3Win, ['_'], ok, hardCoded)]),
	
	%% Add another search
	Search2 = [[jFeed, jf1Win, [<<"Gemma">>], hardCoded]],
	feed_api:add_searches(jFeed3,jf3Win,Search2),
	
	%% Join search should now have 3 elements
	io:format("Searches should now have 3 elements ~p ~n", [feed_api:view_searches(jFeed3,jf3Win)]),
	
	feed_api:add_data(jFeed, "e"),
	
	%% Do another search results should be "Dan and Nope"
	io:format("~p ~n", search_api:search_window(jFeed2, jf2Win, ['_'], ok, hardCoded)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc A pattern example looking for a sequence of events, 1,2,3 and then returning the sum
%%		when the query fires i.e. 6.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_pattern_row_function() ->
	<<"var rowFunction = function(parameters, joins, row, otherRow, sequence, matchRecogniseFirst) {
							ejsLog(\"/tmp/foo.txt\", \"sequence \" + sequence); 
							ejsLog(\"/tmp/foo.txt\", \"parameters[sequence] \" + parameters[sequence]);
							ejsLog(\"/tmp/foo.txt\", \"row \"  + row);
							if(parameters[sequence] == row){
								ejsLog(\"/tmp/foo.txt\", \"match \" + parameters[sequence] + \" \" + row);
								return [row];
							} 
							return []
						 }">>.

create_pattern_reduce_function() ->
	<<"var reduceFunction = function(matches){
								var sum = 0;

								for(var i=0; i<matches.length; i++) {
									var match = matches[i];
									ejsLog(\"/tmp/foo.txt\", \"match =  \" + matches[i]);
									sum += parseInt(match);
								}

								return sum;

						   }">>.

receive_pattern_message()  ->
	receive
		Results ->
			io:format("Received ~p ~n",[Results])
	end.
	
pattern_window() ->
	QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}],
	
	feed_api:start_feed(patternFeed),	
	feed_api:start_window(pattern1Win, patternFeed, examples:create_pattern_row_function(), examples:create_pattern_reduce_function(), QueryParameterList, [1,2,3]),
	
	Pid = spawn(?MODULE, receive_pattern_message, []),
	
	feed_api:subscribe_feed_window(patternFeed, pattern1Win, Pid),
	
	%% Add some data
	feed_api:add_data(patternFeed, 1),
	feed_api:add_data(patternFeed, 2),
	feed_api:add_data(patternFeed, 3).
	
