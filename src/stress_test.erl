-module(stress_test).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% To run stress tests first generate some test data
%% by calling generate_test_data().
%%
%% Then run run_match_test().
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([generate_every_test_data/0, generate_test_data/0, run_match_stress/0, run_every_stress/0, get_response/0, apply_data/0]).
%%
%% API Functions
%%

generate_test_data() ->
	{ok, FileDescriptor} = file:open("stress.txt", [write]), 
	
	lists:map(fun(_Element) -> 
				io:format(FileDescriptor, "~s~n", [window_api:create_single_json("1.00", "10")]) 
			  end, lists:seq(1, 10000)),
	
	io:format(FileDescriptor, "~s~n", [window_api:create_single_json("1.01", "11")]),
	io:format(FileDescriptor, "~s~n", [window_api:create_single_json("1.02", "12")]),
	io:format(FileDescriptor, "~s~n", [window_api:create_single_json("1.03", "13")]),
	io:format(FileDescriptor, "~s~n", [window_api:create_single_json("1.04", "14")]),
	io:format(FileDescriptor, "~s~n", [window_api:create_single_json("1.05", "15")]),
	io:format(FileDescriptor, "~s~n", [window_api:create_single_json("1.06", "16")]),
	io:format(FileDescriptor, "~s~n", [window_api:create_single_json("1.07", "17")]),
	io:format(FileDescriptor, "~s~n", [window_api:create_single_json("1.08", "18")]),
	
	file:close(FileDescriptor).

generate_every_test_data() ->
	{ok, FileDescriptor} = file:open("stress.txt", [write]), 
	
	lists:map(fun(_Element) -> 
				io:format(FileDescriptor, "~s~n", [window_api:create_single_json("1.00", "10")]) 
			  end, lists:seq(1, 10000)),
	
	file:close(FileDescriptor).

get_response() ->
	receive
		Results ->
			io:format("Stress test fired ~p ~n", [Results]),
			get_response()
	end.

for_each_line_in_file(StressPid) ->
    {ok, Device} = file:open("stress.txt", [read]),
    for_each_line(Device, StressPid).

for_each_line(Device, StressPid) ->
    case file:read_line(Device) of
        eof  -> file:close(Device), 
				ok;
        Line -> 
				StressPid ! Line,
                for_each_line(Device, StressPid)
    end.

run_match_stress() ->	
	RowFunction = window_api:create_match_recognise_row_function(),
	ReduceFunction = window_api:create_reduce_function(),
	
	QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}, {matchType, matchRecognise}, {windowType, time}],
	
	feed_api:start_feed(stress),
	
	feed_api:start_window(stressWin, stress, RowFunction, ReduceFunction, QueryParameterList, [0.50]),
	
	Pid = spawn(?MODULE, get_response, []),
	StressPid = spawn(?MODULE, apply_data, []),
	
	feed_api:subscribe_feed_window(stress, stressWin, Pid),
	
	timer:sleep(1000),
	
	Start = os:timestamp(),
	
	io:format("Starting ~n"),
	
	for_each_line_in_file(StressPid),
	
	Time = timer:now_diff(os:timestamp(), Start), 

	io:format("Took ~p microseconds to do 10000 transactions~n", [Time]).

run_every_stress() ->	
	RowFunction = window_api:create_match_recognise_row_function(),
	ReduceFunction = window_api:create_reduce_function(),
	
	QueryParameterList = [{windowSize, 4}, {matchType, every}, {windowType, time}],
	
	feed_api:start_feed(stress),
	
	feed_api:start_window(stressWin, stress, RowFunction, ReduceFunction, QueryParameterList, [0.50]),
	
	Pid = spawn(?MODULE, get_response, []),
	StressPid = spawn(?MODULE, apply_data, []),
	
	feed_api:subscribe_feed_window(stress, stressWin, Pid),
	
	timer:sleep(1000),
	
	Start = os:timestamp(),
	
	io:format("Starting ~n"),
	
	for_each_line_in_file(StressPid),
	
	Time = timer:now_diff(os:timestamp(), Start), 

	io:format("Took ~p microseconds to do 10000 transactions~n", [Time]).

apply_data() ->
	receive 
		{ok, Data} ->
			[H|_T] = re:replace(Data, "\n", ""),

			feed_api:add_data(stress, H),
			apply_data()
	end.