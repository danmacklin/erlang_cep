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

-module(sized_window_gen_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include ("window.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([init/1, start_link/5, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
		 code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Name, RowQuery, ReduceQuery, QueryParameters, Parameters]) ->

	{_NumberOfMatches, _WindowSize, WindowType, _Consecutive, _MatchType, _ResetStrategy} = QueryParameters,
	{ok, JSPort} = js_driver:new(),
	
	js:define(JSPort, RowQuery),
	js:define(JSPort, ReduceQuery),
	
	window_api:import_js(JSPort),

	%% Set-up tick
	case WindowType of
		time ->
			Pid = spawn_link(window_api, generate_tick, [1000, self()]),
			erlang:send_after(1000, Pid, tick);
		size ->
			ok
	 end,
	
	{ok, #state{name = Name, position = 0, results = dict:new(), matches = [[]], timingsDict = dict:new(), rowQuery = RowQuery, 
				reduceQuery = ReduceQuery, queryParameters = QueryParameters, jsPort = JSPort, pidList = [], sequenceNumber=0, 
				parameters = Parameters, searches = []}}. 

start_link(Name, RowQuery, ReduceQuery, QueryParameters, Parameters) -> 
	gen_server:start_link(?MODULE, [Name, RowQuery, ReduceQuery, QueryParameters, Parameters], []).

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({isSubscribed, Pid}, _From, State=#state{pidList = PidList}) ->
    Reply = case lists:member(Pid, PidList) of
				true ->
					{ok, true};
				false ->
					{ok, false}
			end,
    {reply, Reply, State};

handle_call({getSearches}, _From, State=#state{searches = Searches}) ->
	{reply, {ok, Searches}, State};

handle_call({search, SearchParameter}, _From, State) ->
	{reply, search_api:search(SearchParameter, State), State};

handle_call(Request, _From, State) ->
	io:format("Unexpected call message ~p ~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({add_data, Data, Joins}, State) ->
	{noreply, window_api:do_add_data(Data, State, Joins)};

handle_cast({subscribe, Pid}, State) ->
	{noreply, window_api:do_subscribe(Pid, State)};

handle_cast({unSubscribe, Pid}, State) ->
	{noreply, window_api:do_unSubscribe(Pid, State)};

handle_cast({addSearches, NewSearches}, State = #state{searches = Searches})->
	{noreply, State#state{searches=lists:append(NewSearches, Searches)}};

handle_cast({removeSearches, RemoveSearches}, State = #state{searches = Searches})->
	
	NewSearches = lists:foldl(fun(Element, Acc) ->
									  lists:delete(Element, Acc)
							  end, Searches, RemoveSearches),
	
	{noreply, State#state{searches = NewSearches}};

handle_cast({stop}, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
	io:format("Unexpected cast message ~p ~n", [Msg]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% Handle the tick event
handle_info(tick, State) ->
    {noreply, window_api:clock_tick(State)}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	io:format("window shutting down : ~p ~p ~n", [State#state.name, Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
