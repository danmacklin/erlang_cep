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

-module(expiry_api).

-ifdef(TEST).
	-include_lib("eunit/include/eunit.hrl").
-endif.

-include("window.hrl").

%%
%% Exported Functions
%%
-export([expire/3, filter/2, add_to_expiry_dict/3, expire_from_expiry_dict/6, expiry_list_swap/2, expiry_list_reset_old/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Timed Window code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Add a new element to the new list within the timings dict
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_to_expiry_dict(TimingsDict, Element, CurrentSecond) ->
	case dict:find(CurrentSecond, TimingsDict) of
		error ->
			NewRecord = #timingData{oldData=[], newData=[Element]},
			dict:store(CurrentSecond, NewRecord, TimingsDict);
	    {ok, ExistingValue} ->
			NewList = [Element] ++ ExistingValue#timingData.newData,
			dict:store(CurrentSecond, ExistingValue#timingData{newData=NewList}, TimingsDict)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Expire data from the current seconds timings dict
%% @TODO : Could be rolled into add_to_expiry dict but for now
%%        keep it seperate for simplicity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expire_from_expiry_dict(TimingsDict, CurrentSecond, MatchList, ResultsDict, Now, WindowSize) ->
	case dict:find(CurrentSecond, TimingsDict) of
		error ->
			{TimingsDict, MatchList, ResultsDict};
	    {ok, ExistingValue} ->
			{ToBeKept, ToBeDeleted} = expire(ExistingValue#timingData.oldData, Now, WindowSize),
			%% Delete from matchList
			FilteredMatchList = filter_match_list(MatchList, ToBeDeleted),
			FilteredResultsDict = remove_from_results_dict(ToBeDeleted, ResultsDict),
			NewTimingRecord = ExistingValue#timingData{oldData=ToBeKept},
			{dict:store(CurrentSecond, NewTimingRecord, TimingsDict), FilteredMatchList, FilteredResultsDict}
	end.

filter_match_list(MatchList, ToBeDeleted) ->
	%%[[1,2,3], [2,3,4]]  [{1, timestamp},{2, timeStamp},{3, timestamp}]
	lists:foldl(
				fun (Matches, MatchesAcc) ->
					MatchesAcc ++ [
						lists:foldl(fun (DeleteElement, MatchAcc) ->
										lists:delete(DeleteElement, MatchAcc)		 
									end, Matches, ToBeDeleted) ]
				end, [], MatchList).
	
%% @doc Removes expired data from the results dict
remove_from_results_dict([], ResultsDict) ->
	ResultsDict;

remove_from_results_dict(ToBeDeleted, ResultsDict) ->
	lists:foldl(fun({SeqNo, _TimeStamp}, Dict) -> 
						dict:erase(SeqNo, Dict)
				end, ResultsDict, ToBeDeleted).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Swap the new list to the old list within the timings dict
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expiry_list_swap(TimingsDict, CurrentSecond) ->
	case dict:find(CurrentSecond, TimingsDict) of
		error ->
			TimingsDict;
		{ok, TimingsRecord} ->
			OldList = TimingsRecord#timingData.newData,
			NewList = TimingsRecord#timingData.oldData,
			dict:store(CurrentSecond, TimingsRecord#timingData{newData=[], oldData= NewList ++ OldList}, TimingsDict)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Reset the oldList within the TimingsDict
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expiry_list_reset_old(TimingsDict, CurrentSecond, MatchList, ResultsDict) ->
	case dict:find(CurrentSecond, TimingsDict) of
		error ->
			{TimingsDict, MatchList, ResultsDict};
		{ok, TimingsRecord} ->
			
			%% Remove any matches from oldData from the resultsDict and match list
			OldList = TimingsRecord#timingData.oldData,
				
			{dict:store(CurrentSecond, TimingsRecord#timingData{oldData=[]}, TimingsDict),
			 remove_from_match_list(MatchList, OldList),
			 remove_from_results_dict(OldList, ResultsDict)}
	end.

remove_from_match_list(MatchList, []) ->
	MatchList;

remove_from_match_list(MatchList, OldList) ->
	
	lists:foldl(fun({SeqNo, _TimeStamp}, Acc) ->
					window_api:remove_match(Acc, SeqNo)
				end, MatchList, OldList).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc The timed dict holds an old and new list for each second within the window.
%% 		These lists hold the values in the format [{SeqNo, TimeStamp}, {SeqNo, TimeStamp}]
%% 		This function takes in this list and returns {ListOfElementsThatHavePassedExpiryTest (to be kept), 
%% 		ListOfElementsThatHaveFailedExpiryTest (to be deleted)}
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expire(List, Now, WindowSize) ->
	expire(List, Now, WindowSize, [], []).

expire([], _Now, _WindowSize, NonExpired, Expired) ->
	{lists:reverse(NonExpired), lists:reverse(Expired)};

expire([{_SequenceNumber, TimeStamp} = H | T], Now, WindowSize, NonExpired, Expired) ->
	case timer:now_diff(Now, TimeStamp) > (WindowSize * 1000000) of
		true ->
			expire(T, Now, WindowSize, NonExpired, [H] ++ Expired);
		false ->
			expire(T, Now, WindowSize, [H] ++ NonExpired, Expired)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc A function to filter FilterList from Original
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filter(FilterList, OriginalList) ->
	lists:filter(fun(X) -> 
						 case lists:member(X, FilterList) of
							 true ->
								 false;
							 false ->
								 true
						 end
				 end, OriginalList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).

match_list_filter_test() ->
	MatchList = [[1,2,3], [2,3,4]],
	DeleteList = [1,2],
	?assertEqual([[3], [3,4]], filter_match_list(MatchList, DeleteList)).

%% @doc On a second tick move all of the elements in the new list to the old list
add_to_expiry_dict_test() ->
	NewDict = add_to_expiry_dict(add_to_expiry_dict(dict:new(), {1, now}, 1), {2,now}, 1),
	Record = dict:fetch(1, NewDict),
	?assertEqual([{2,now},{1,now}], Record#timingData.newData),
	
	SwappedDict = expiry_list_swap(NewDict, 1),

	SwappedRecord = dict:fetch(1, SwappedDict),

	?assertEqual([], SwappedRecord#timingData.newData),
	?assertEqual([{2,now},{1,now}], SwappedRecord#timingData.oldData),
	
	%% Test condition where we are attempting to swap the new and old lists for a second where there is no data.  Essentially a no-op.
	NotSwappedDict = expiry_list_swap(SwappedDict, 2),
	?assertEqual(SwappedDict, NotSwappedDict).
	
	%% Test reset of old list
	%%ResetRecord = expiry_list_reset_old(SwappedDict,1),
	%%?assertEqual([], ResetRecord#timingData.oldData),
	
	%% Test condition where we are trying to rest the old list for a second where the is no data.  Again a no-op
	%%NotResetDict = expiry_list_reset_old(SwappedDict,2),
	%%?assertEqual(SwappedDict, NotResetDict).
	
build_expiry_list(Now) ->
	[{1, Now}, {2, Now}, {3,Now}, {4, Now}].

build_expiry_list(Now, Now2) ->
	[{1, Now}, {2, Now}, {3,Now2}, {4, Now2}].

no_expire_test() ->
	Now = os:timestamp(),
	ExpiryList = build_expiry_list(Now),
	?assertEqual({ExpiryList, []}, expire(ExpiryList, Now, 1)).

expire_test() ->
	Now = os:timestamp(),
    ok = timer:sleep(2000),
    Now2 = os:timestamp(),
	ExpiryList = build_expiry_list(Now),
	?assertEqual({[], ExpiryList}, expire(ExpiryList, Now2, 1)).

some_expire_test() ->
	Now = os:timestamp(),
    ok = timer:sleep(2000),
    Now2 = os:timestamp(),
	ExpiryList = build_expiry_list(Now, Now2),
	?assertEqual({[{3, Now2}, {4, Now2}], [{1, Now}, {2, Now}]}, expire(ExpiryList, Now2, 1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc filter filterList from List
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filter_test() ->
	Now = os:timestamp(),
	ok = timer:sleep(1500),
    Now2 = os:timestamp(),
	
	List = build_expiry_list(Now, Now2),
	FilterList = [{3, Now2}, {4, Now2}],
	?assertEqual([{1, Now}, {2, Now}], filter(FilterList, List)).

-endif.