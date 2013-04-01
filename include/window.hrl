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

-record(state, {name			:: atom(),
				results			:: dict(),
				matches			:: list(integer()), 
				timingsDict		:: dict(), 
				position		:: integer(), 
				rowQuery		:: binary(), 
				reduceQuery 	:: binary(), 
				queryParameters	:: atom | tuple(), 
				jsPort			:: pid(), 
				pidList			:: list(pid()), 
				sequenceNumber	:: integer(), 
				parameters		:: list(any()), 
				searches		:: list()}).

-record(timingData, {oldData	:: list(integer()), 
					 newData	:: list(integer())}).