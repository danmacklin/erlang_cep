%%% -------------------------------------------------------------------
%%% Author  : dan
%%% Description :
%%%
%%% Created : 9 Dec 2012
%%% -------------------------------------------------------------------
-module(feed_gen_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("feed.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([]).

%% gen_server callbacks
-export([start_link/1,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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
init([Name]) ->
	%%io:format("Starting feed_gen_server ~p ~n", [Name]),
    {ok, #feedState{name=Name, windowPids=[], searchDict=dict:new()}}.

start_link(Name) -> 
	gen_server:start_link({local, feed_api:get_feed_genserver_name(Name)}, ?MODULE, [Name], []).

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

%% Returns the state for unit tests.
handle_call({getState}, _From, State) ->
    Reply = ok,
    {reply, Reply, State};

handle_call({search, WindowName, SearchParameter}, _From, State) ->
	{reply, search_api:do_search(WindowName, SearchParameter), State};

handle_call({viewSearches, WindowName}, _From, State=#feedState{searchDict=SearchDict}) ->
	%%{reply, window_api:view_searches(WindowName), State};
	{reply, feed_api:do_view_searches(SearchDict, WindowName), State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

%% In order to test the supervisor hierarchy I have created this cast
%% which will crash the genserver.
handle_cast({generateError}, _State) ->
	p=y;

handle_cast({startWindow, WindowName, FeedName, RowFunction, ReduceFunction, QueryParameters, Parameters}, State=#feedState{windowPids = WindowPids}) ->	
	Pid = window_sup:start_child(FeedName, WindowName, RowFunction, ReduceFunction, QueryParameters, Parameters),
	{noreply, State#feedState{windowPids = [{WindowName,Pid} | WindowPids]}};

handle_cast({stopWindow, WindowName}, State=#feedState{windowPids = WindowPids}) ->
	{_WindowName, Pid} = lists:keyfind(WindowName, 1, WindowPids),
	
	gen_server:cast(Pid, {stop}),
	
	{noreply, State#feedState{windowPids = lists:keydelete(WindowName, 1, WindowPids)}};

handle_cast({addData, Data}, State=#feedState{windowPids = WindowPidList, searchDict=SearchDict}) ->
	feed_api:do_add_data(Data, WindowPidList, SearchDict),
	{noreply, State};

handle_cast({subscribe, WindowName, Pid}, State) ->
	feed_api:do_subscribe_feed_window(WindowName, Pid),
	{noreply, State};

handle_cast({addSearches, WindowName, Searches}, State=#feedState{searchDict=SearchDict}) ->
	%%window_api:add_searches(WindowName, Searches),
	{noreply, State#feedState{searchDict=feed_api:do_add_searches(SearchDict, WindowName, Searches)}};

handle_cast({removeSearches, WindowName, Searches}, State=#feedState{searchDict=SearchDict}) ->
	%%window_api:remove_searches(WindowName, Searches),
	{noreply, State#feedState{searchDict=feed_api:do_remove_searches(WindowName, Searches, SearchDict)}};
	
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	%%io:format("feed_genserver_terminating ~p ~n", [self()]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.