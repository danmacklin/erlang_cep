%%% -------------------------------------------------------------------
%%% Author  : dan
%%% Description :
%%%
%%% Created : 9 Dec 2012
%%% -------------------------------------------------------------------
-module(feed_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1, start_link/1]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([FeedName]) ->
	
	FeedGen = {feed_gen_server,
					{feed_gen_server, start_link, [FeedName]}, 
				  	transient, 5000, worker, [feed_gen_server]},
	
	WindowSup = {window_sup,
					{window_sup, start_link, [FeedName]},
					transient, 5000, supervisor, [window_sup]},
	
    {ok,{{one_for_all,1,1}, [FeedGen, WindowSup]}}.

start_link(FeedName) ->
	FeedAtom = list_to_atom(string:concat(atom_to_list(FeedName), "Controller")),
	supervisor:start_link({local, FeedAtom}, ?MODULE, [FeedName]).