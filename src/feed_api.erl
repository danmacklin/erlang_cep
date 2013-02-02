%% Author: dan
%% Created: 9 Dec 2012
%% Description: TODO: Add description to feed_api
-module(feed_api).

-include_lib("eunit/include/eunit.hrl").

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start_feed/1, get_feed_genserver_name/1, crash_feed_genserver/1, start_window/5, stop_window/2,
		 add_data/2, do_add_data/2, subscribe_feed_window/3, do_subscribe_feed_window/2, receive_message/1]).

%%
%% API Functions
%%

%% @doc Starts a supervision hierarchy for a new feed
start_feed(FeedName) ->
	{ok, Pid} = feed_sup:start_link(FeedName),
	Pid.

get_feed_genserver_name(FeedName) ->
	list_to_atom(string:concat(atom_to_list(FeedName), "Genserver")).

%% @doc Used to test the supervisor hierarchy
crash_feed_genserver(FeedName) ->
	gen_server:cast(get_feed_genserver_name(FeedName), {generateError}).

%% @doc Start a new window for this feed.
start_window(WindowName, FeedName, RowFunction, ReduceFunction, QueryParameterList) ->
	QueryParameters = query_parameter_api:get_parameters(QueryParameterList),
	gen_server:cast(get_feed_genserver_name(FeedName), {startWindow, WindowName, FeedName, RowFunction, ReduceFunction, QueryParameters}).

%% @doc Stop a window
stop_window(FeedName, WindowName) ->
	gen_server:cast(get_feed_genserver_name(FeedName), {stopWindow, WindowName}).

%% @doc Subscribe to a feeds window
subscribe_feed_window(FeedName, WindowName, Pid) ->
	gen_server:cast(get_feed_genserver_name(FeedName), {subscribe, WindowName, Pid}).

%% @doc Called by feed_genserver to do the subscribe
do_subscribe_feed_window(WindowName, Pid) ->
	window_api:subscribe(WindowName, Pid).

%% @doc Add data to a feed
add_data(FeedName, Data) ->
	gen_server:cast(get_feed_genserver_name(FeedName), {addData, Data}).

%% @doc Called by feed_gen_server to send the data to each window
do_add_data(Data, WindowPidsList) ->
	%% Send the data to each window
	lists:foreach(fun({_WindowName, Pid}) ->
						  gen_server:cast(Pid, {add_data, Data})
				  end, WindowPidsList).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

receive_message(Pid) ->
	receive
		Results ->
			timer:sleep(1000),
			Pid ! Results
	end.

%% @doc This test sets up a feed with a standard window looking for the volume to 
%% 		increase each time.  It should fire the reduce function that will calculate
%% 		the average volume.
%% @end

standard_end_to_end_3_elements_test() ->
	erlang_js:start(),
	
	application:start(erlang_cep),
	
	Data = window_api:create_json([{"1.01", "10"}, {"1.02", "11"}, {"2.00", "12"}]),
	RowFunction = window_api:create_standard_row_function(),
	ReduceFunction = window_api:create_reduce_function(),
	
	QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}],
	
	start_feed(testFeed),
	
	start_window(testWinFeed, testFeed, RowFunction, ReduceFunction, QueryParameterList),
	
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
	end.

%% @doc This test uses a matchRecognise function to check that the volume is increasing each time within the window.
match_recognise_end_to_end_3_elements_test() ->	

	Data = window_api:create_json([{"1.01", "14"}, {"1.02", "15"}, {"2.00", "16"}]),
	RowFunction = window_api:create_match_recognise_row_function(),
	ReduceFunction = window_api:create_reduce_function(),
	
	QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}, {matchType, matchRecognise}],
	
	start_feed(testFeedMatchRecognise),
	
	start_window(testWinFeedMatchRecognise, testFeedMatchRecognise, RowFunction, ReduceFunction, QueryParameterList),
	
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

%% @doc A timed window test
time_match_recognise_end_to_end_3_elements_test() ->	
	Data = window_api:create_json([{"1.01", "14"}, {"1.02", "15"}, {"2.00", "16"}]),
	RowFunction = window_api:create_match_recognise_row_function(),
	ReduceFunction = window_api:create_reduce_function(),
	
	QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}, {matchType, matchRecognise}, {windowType, time}],
	
	start_feed(time),
	
	start_window(timeWin, time, RowFunction, ReduceFunction, QueryParameterList),
	
	Pid = spawn(?MODULE, receive_message, [self()]),
	
	subscribe_feed_window(time, timeWin, Pid),
	
	lists:foreach(fun (DataElement) ->
					ok = timer:sleep(1000),
					add_data(time, DataElement)
				  end, Data),
	receive
		Results ->
			io:format("fired~n"),
			?assertEqual(15, Results)
	after 4000 ->
			io:format("Timed out ~n"),
			p=y
	end.

%% @doc A timed every window test
time_every_end_to_end_3_elements_test() ->	
	Data = window_api:create_json([{"1.01", "14"}, {"1.02", "15"}, {"2.00", "16"}]),
	RowFunction = window_api:create_match_recognise_row_function(),
	ReduceFunction = window_api:create_reduce_function(),
	
	QueryParameterList = [{windowSize, 2}, {matchType, every}, {windowType, time}],
	
	start_feed(every),
	
	start_window(everyWin, every, RowFunction, ReduceFunction, QueryParameterList),
	
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