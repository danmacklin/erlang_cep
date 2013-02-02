%% Author: dan
%% Created: 13 Nov 2012
%% Description: TODO: Add description to query_parameter_api
-module(query_parameter_api).

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([get_parameters/1]).

%%
%% API Functions
%%

%% @doc The system has the following query parameters
%% 		QueryParameters = {4, consecutive, standard, reset, 3},
%% 		{NumberOfMatches, WindowSize, WindowType, Consecutive, MatchType, ResetStrategy} = QueryParameters,

%% 		windowType - size / time size = x no elements, time = window based on time). default = size
%% 		windowSize - The size of the the window in seconds if time, or just size. Needs to be set or error
%% 		consecutive - [consecutive / nonConsecutive] Do matches have to follow each other default Consecutive.  default consecutive
%% 		matchType - [standard / matchRecognise / every] - MatchRecognize queries are used to look for patterns based on the previous data.  
%%              	 Standard look at the incomming data only.  
%%              	 every adds every match to the matchlist (reduce run every window rollover)
%%              	 default standard
%% 		resetStrategy - [restart / noRestart] - After a match restart deletes all matches and starts again, noRestart carries on.  default restart
%% 		numberOfMatches - The number of rows this query has to run before it matches.  Needs to be set or error

%% 		Parameters come in as a list need to convert to a tuple for efficient pattern matching
%% 		[{atom, parameter}]

%% 		This is the minimum
%% 		[{numberOfMatches, 5}, {windowSize, 6}]

%% 		This is a more specialised example
%% 		[{numberOfMatches, 5}, {windowSize, 6}, {windowType, size}]

%% 		All unrecognised parameters need to generate an error.
%%	@end
get_parameters(ParameterList) ->
	
	case convert_parameters(ParameterList) of
		error ->
			error;
		{error, _, _, _, _, _} ->
			error;
		{_, error, _, _, _, _} ->
			error;
		{_NumberOfMatches, _WindowSize, _WindowType, _Consecutive, _MatchType, _ResetStrategy} = Results ->
				Results
	end.

convert_parameters([]) ->
	error;
	
convert_parameters(ParameterList) when length(ParameterList) < 2 ->
	error;

convert_parameters(ParameterList) ->
	NumberOfMatches = case get_parameter(ParameterList, numberOfMatches) of
						  error ->
							  case get_parameter(ParameterList, matchType) of
								  every ->
									  0;
								  _ ->
									  error
							  end;
						  Value when is_integer(Value) ->
							  Value;
						  _ ->
							  error
					  end,

	WindowSize = case get_parameter(ParameterList, windowSize) of
					 	error ->
							error;
					 	WindowValue when is_integer(WindowValue) ->
							WindowValue;
					 	_ ->
							error
				 end,
	
	Consecutive = case get_parameter(ParameterList, consecutive) of
					  error ->
						  consecutive;
					  consecutive ->
						  consecutive;
					  _ ->
						  nonConsecutive
				  end,
	
	MatchType = case get_parameter(ParameterList, matchType) of
					error ->
						standard;
					standard ->
						standard;
					every ->
						every;
					_ ->
						matchRecognise
				end,
	
	ResetStrategy = case get_parameter(ParameterList, resetStrategy) of
						error ->
							restart;
						restart ->
							restart;
						_ ->
							noRestart
					end,

	WindowType = case get_parameter(ParameterList, windowType) of
					error ->
						size;
					 size ->
						 size;
					_ ->
						time
				end,

	{NumberOfMatches, WindowSize, WindowType, Consecutive, MatchType, ResetStrategy}.
	
get_parameter(ParameterList, ParameterName) ->
	case lists:keyfind(ParameterName, 1, ParameterList) of
		false ->
			error;
		{_Name, Value} ->
			Value
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_parameter_test() ->
	ParameterList = [],
	?assertEqual(error, query_parameter_api:get_parameters(ParameterList)).

invalid_parameter_1_test() ->
	ParameterList = [{no,5}],
	?assertEqual(error, query_parameter_api:get_parameters(ParameterList)).

invalid_parameter_2_test() ->
	ParameterList = [{no,5}, {data, 10}],
	?assertEqual(error, query_parameter_api:get_parameters(ParameterList)).

invalid_parameter_string_windowsize_test() ->
	ParameterList = [{numberOfMatches, 5}, {windowSize, "10"}],
	?assertEqual(error, query_parameter_api:get_parameters(ParameterList)).

invalid_parameter_string_numberOfMatches_test() ->
	ParameterList = [{numberOfMatches, "5"}, {windowSize, 10}],
	?assertEqual(error, query_parameter_api:get_parameters(ParameterList)).

good_parameters_test() ->
	ParameterList = [{numberOfMatches, 5}, {windowSize, 10}],
	?assertEqual({5, 10, size, consecutive, standard, restart}, query_parameter_api:get_parameters(ParameterList)).

good_parameters_every_test() ->
	ParameterList = [{windowSize, 10}, {matchType, every}],
	?assertEqual({0, 10, size, consecutive, every, restart}, query_parameter_api:get_parameters(ParameterList)).

good_parameters_all_test() ->
	ParameterList = [{numberOfMatches, 5}, {windowSize, 10}, {consecutive, consecutive}, {matchType, standard}, {resetStrategy, restart}, {windowType, size} ],
	?assertEqual({5, 10, size, consecutive, standard, restart}, query_parameter_api:get_parameters(ParameterList)).

good_parameters_all_2_test() ->
	ParameterList = [{numberOfMatches, 5}, {windowSize, 10}, {consecutive, nonConsecutive}, {matchType, matchRecognise}, {resetStrategy, noRestart}, {windowType, time} ],
	?assertEqual({5, 10, time, nonConsecutive, matchRecognise, noRestart}, query_parameter_api:get_parameters(ParameterList)).