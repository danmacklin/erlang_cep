%% Author: dan
%% Created: 5 Nov 2012
%% Description: TODO: Add description to sized_window_api
-module(search_api).

%%
%% Include files
%%

-include("window.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([search/2, search_window/5, do_search/2, perform_substitution/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Perform a search within a window.  The search parameter is a list, where '_' is a wild card
%%		for example ["GOOG", '_', 12]  will find all of the stocks at any price with a volume of 12. 
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search_window(FeedName, WindowName, SearchParameter, _Row, hardCoded) ->
	gen_server:call(feed_api:get_feed_genserver_name(FeedName), {search, WindowName, SearchParameter});

%% Perform a Search that requires json substitution = ["Obj.symbol", "Obj.volume", "Obj.price"]
search_window(FeedName, WindowName, SearchParameter, Row, json) ->
	gen_server:call(feed_api:get_feed_genserver_name(FeedName), {search, WindowName, perform_substitution(Row, SearchParameter)}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Sometimes you want to dynamically build a search query based on the incomming data.  
%%		This function allows you to build a search string that allows you to extract values from the input json document (ROW)
%%		and insert them into the query string.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
perform_substitution(Row, Parameters) ->
	ParsedJson = mochijson2:decode(Row),
	[substitute(ParsedJson, X) || X <- Parameters].

%% Perform list substitutuion - WildCard
substitute(_Row, '_') ->
	'_';

%% Perform list substitution from Json look-up
substitute(Row, Element) ->
	parse_json:destructure(Element, Row).

%% Called from feed genserver to perform search within window process
do_search(WindowName, SearchParameter) ->
	[{_WindowName,WindowPid}] = ets:lookup(window_ets, WindowName),
	gen_server:call(WindowPid, {search, SearchParameter}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%% @doc Called from within a sized_window_gen_server.
%%		Search the results dictionary within a window for a range of values [val1, '_' , val2]
%%      where _ = don't care and the list has to be the size of the result list.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search(SearchParameters, _State=#state{results=Results}) ->
	runSearch(SearchParameters, Results).

%% Actually do the search
runSearch(SearchParameters, Results) ->
	dict:fold(fun (_Key, {_Row,Value}, Acc) ->
				case searchValue(SearchParameters, Value) of
					true ->
						%% Do not return empty rows as part of searches.  TODO - How do empty results get in here.
						case Value of 
							[[]] ->
								Acc;
							Value ->
								[Value] ++ Acc
						end;
					false ->
						Acc
				end
 			end, [], Results).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Take a list of search parameters [val1, '_', val2] and check against a list of results to
%%      return true if the search parameters match and false otherwise.
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Searching over an empty window gives an empty result.
searchValue(_SearchParameters, []) ->
	false;

searchValue(SearchParameters, Values) ->
	SearchList = lists:zipwith(fun(SP,Val) -> match(SP, Val) end, SearchParameters, Values),
	
	lists:foldl(fun(Value, Acc) ->
						case Acc of
								false ->
									Acc;
								true ->
									Value
						end
				end, true, SearchList).

match('_', _Value) ->
	true;

match(Parameter, Value) when Parameter == Value ->
	true;

match(_Parameter, _Value) ->
	false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Search function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_results_dict() ->
	dict:store(2, {"", ["Goog", 1.01, 13]}, dict:store(1, {"", ["Goog", 1.00, 12]}, dict:new())).

do_pass_search_test() ->	
	?assertEqual([["Goog", 1.01, 13], ["Goog", 1.00, 12]], 
				 	runSearch(["Goog", '_', '_'], create_results_dict())). 

do_pass_search_13_test() ->
	?assertEqual([["Goog", 1.01, 13]], runSearch(["Goog", '_', 13], create_results_dict())). 

do_pass_search_fail_test() ->
	?assertEqual([], runSearch(["Goog1", '_', 13], create_results_dict())). 

substitution_test() ->
	Json1 = window_api:create_single_json("1.01", "10"),
	?assertEqual([<<"GOOG">>,'_',1.01], perform_substitution(Json1, ["Obj.symbol", '_' , "Obj.price"])).