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

-module(window_sup).

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
-export([init/1, start_link/1, start_child/6]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------
-define(SERVER, ?MODULE).

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% ====================================================================
%% External functions
%% ====================================================================

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
	
	ChildSpec = {sized_window_gen_server,
					{sized_window_gen_server, start_link, []}, 
				  	transient, 5000, worker, [sized_window_gen_server]},
	
	{ok, {{simple_one_for_one, 5, 1},[ChildSpec]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_link(FeedName) ->
	supervisor:start_link({local, FeedName}, ?MODULE, []).

%%TODO What if someone registers a name that is already registered?
start_child(FeedName, Name, RowQuery, ReduceQuery, QueryParameters, Parameters) ->
	{ok,Pid} = supervisor:start_child(FeedName,[Name, RowQuery, ReduceQuery, QueryParameters, Parameters]),
	
	ets:insert(window_ets, {Name, Pid}),
	
	Pid.