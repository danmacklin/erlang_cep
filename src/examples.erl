-module(examples).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A few examples to get you used to using this system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Exported Functions
%%
-export([every_window/0, create_single_sales_json/3, receive_message/2]).
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
	
receive_message(Pid, No) ->
							 ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Set-up a feed with a five second window, performing
%% 		an every match.
%%
%% 		A typical use case for this kind of window would
%% 		be to store sales data, and then average it up
%% 		every to seconds.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
	<<"var rowFunction = function(parameters, joins, row, otherRow, first){
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