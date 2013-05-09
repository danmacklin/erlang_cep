#erlang_cep v0.2 Beta

A simple CEP (Complex Event Processing) engine written in erlang OTP inspired by esper and CouchDB. 

##Features

* Simple API.
* CEP rules programmed in javascript or pure erlang.
* Support for many data feeds.  Each feed having none or many time or size based windows. 
* Three step CEP rules.  Row Functions filter data on entry to a window. Configuration sets the size of window and number of matches. Reduce Functions aggregate data when a CEP rule fires.
* Standard, MatchRecognize and Every CEP rules.
* Configurable pattern recognition.
* Searchable windows using a list based query API.
* Join data from multiple windows. 
* Publish and subscribe mechanism notifies interested processes when CEP rules match.

##How Does it Work?

The feed_api is used to create Feeds and Windows.  

Each feed can have none or many windows.

A Feed is a stream of json encoded data (other formats are possible, but might need some more testing in this version).

A Window is an in-memory data storage abstraction.  Each window has a Row Function, Reduce Function and associated configuration that can be implemted in javascript or erlang. 

A Row Function is a filter.  Data is added to the window if it passes the filter.

A Reduce Function is a data aggregator. It aggregates data when a CEP rule fires (for example averaging data or performing output formatting).   


A good example is a Stock Market Feed with a time based window.  The window is programmed to fire when three trades with the symbol GOOG and a volume of 100 shares or more make it into the window.  When the window fires the Reduce Function runs and averages the sale prices.  Finally the publish and subscribe mechanism distributes the average price and stock symbol to interested processes.

##Query Types

erlang_cep supports three types of query.

* "Standard" - Performs simple matches.  I.e. match every google trade with a sale price > £10.
           
* "matchRecognise" - Perform matches that make a progression.  I.e. match every google trade with a sale price greater than £10, where the next sale price is greater than the previous sell price.  Fire when this progression happens 3 times in a row.
				 
* "every" - Performs standard matches where the results are output at a regular interval.  I.e in a 60 second window give me the average sale price for all Google trades.

##Window Types

erlang_cep supports two types of window.

* size - Sized windows have a fixed size.  Each time data is added the position within the window is incremented. Once the position equals the window size the position resets to 0 and the data within the window is overwritten and expired.  If you think of it as a ring buffer you won't be too far away. 
       
* time - Time based windows expire data after a period of time.  For example a 60 second timed window stores any number of data points added within a 60 second period.
       
##Window Configuration

Windows are configured through a list of tuples.  Version 0.2 supports the following configuration parameters where the first element in the tuple is the parameter name, the second is a value or atom.

* {numberOfMatches, Numeric} 	- The number of matches required to fire the Reduce Function.
* {WindowSize, Numeric} 	- The size of the window (number of elements for size based windows, time in seconds for time based windows), 
* {WindowType , size / time} 	- The type of the window size or time. 
* {Consecutive, consecutive / nonConsecutive} - Specifies whether matches need to be made up from consecutive or nonConsecutive data elements.  For example if this flag is set to consecutive, and numberOfMatches is set to 3, then the reduce function will fire if the Row Function for three consecutive elements match			  
* {matchType, standard / matchRecognise / every}
* {resetStrategy, restart / noRestart}	- After the Reduce Function runs should the window be reset or carry on.

The default WindowType is size.
The default Consecutive parameter is consecutive.
The default matchType is standard.
The default resetStrategy is restart.

If a query fails to parse an error atom is returned.

Some examples :-

Set up a standard four element size window, which will fire it's reduce function after three consecutive matches and then restart.

    QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}]

Set up a matchRecognise four element window, which will fire it's reduce function after three consecutive matches and then restart

    QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}, {matchType, matchRecognise}],

Set up a matchRecognised four second time based window, which will fire it's reduce function after three consecutive matches
and then restart.

    QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}, {matchType, matchRecognise}, {windowType, time}],

##Row Functions

Row Functions are defined in javascript as binaries or pure erlang functions.  There are three types of Row Function "standard" / "every" and "matchRecognise".

###Standard Row Functions 

"Standard" Row Functions are used to filter data on entry to a window.  They can be implemented in javascript or as pure erlang functions.

###Javascript Row Functions

Each javascript Row Function must accept the following parameters :-

1. parameters - An array of values initialised when the window is first started. Use these parameters to build functions without hard coded values. i.e. if (price > parameters[0]) rather than if (price > 1.00)
2. joins - An array or arrays of joined data.  [ [FeedName, WindowName, [ Joined Rows ]] ].  Each joined row is an array of one or more elements.
3. row - A String containing the new data.
4. otherRow - A string holding data from the previous match.  Only used in "matchRecognise" windows, where the new data is compared against the last successful match, rather than a hard coded rule.
5. sequence - The current number of matches within this window.  Use this number and an array of parameters to perform complex matches.
6. matchRecogniseFirst - A boolean set to true if this is the initial check within a matchRecognise window, false if checking data against previous values.

As an example this javascript function will match if the price is greater than 1.00.

    <<"var rowFunction = function(parameters, joins, row, otherRow, sequence, matchRecogniseFirst){
    							
    							var myObject = JSON.parse(row);
    							symbol = myObject.symbol;
    							price = myObject.price;
    							volume = myObject.volume;
    
    							if (price > 1.00){
    								return [symbol, price, volume];
    							}
    							
    							return []}">>.
							

To debug a javascript row function use

	ejsLog("/tmp/foo.txt", "Hello");


If the Row Function fails to match it must return an empty list.

The results of a Row Function must be a list.  The order of the list is important as it dictates how the data is stored within the window results.  Row Functions, searches and joins must respect this order.

When using joins, the javascript code can be simplified by using the Join object.  

Create a join object passing the joins parameter and optionally the name of the join feed / window if you want to restrict the join.
  
var j = new Join(joins);

Use the exists(value, position) function to Search the joins array for a value at position pos. 

The source code for the javascript API can be found in /src/js.  If you add any other javascript functions to this file / directory they will be loaded into the javascript vm each time a window starts.

Here's an example.

	var rowFunction = function(parameters, joins, row, otherRow, sequence, matchRecogniseFirst){
		var j = new Join(joins);

		if (j.exists(parameters[0], 0)){
			return [\"Yes\"];
		}
		
	return [\"Nope\"]}

This example returns Yes if the value of parameters[0] is found at position 0.

Here's example of a javascript row function with a join looking for data from a feed called feed1 and window called window1.


	var rowFunction = function(parameters, joins, row, otherRow, sequence, matchRecogniseFirst){
		var j = new Join(joins, 'feed1', 'window1');

		if (j.exists(parameters[0], 0)){
			return [\"Yes\"];
		}
		
	return [\"Nope\"]}


###Javascript "MatchRecognise" Row Functions

"MatchRecognise" Row Functions are used to find complex progressive patterns in data. Typically a MatchRecognise Row Function is called from two perspectives.  Initially the "matchRecogniseFirst" parameter 
is set to true and the function acts as as a threshold check.  In this mode the function is a filter making sure that the new row data can be added to the Window. On subsequent calls the matchRecogniseFirst 
parameter is set to false, and the data from the previous match is passed into the "otherRow" variable facilitating a comparison between the latest
data and the last good match.  The matchRecogniseFirst parameter is set to false when matched data expires from the window, or a query fires (if resetStrategy is set to restart).

When the matchRecogniseFirst parameter is set to true, the function must return the data which will be passed to the window.

When the matchRecogniseFirst parameter is set to false, the function is being called from the perspective of "matchRecognise" so only needs to return true if a test
has passed, or false otherwise.  

An example javascript MatchRecognise Row Function

    <<"var rowFunction = function(parameters, joins, row, otherRow, sequence, matchRecogniseFirst){
    							
    							var myObject = JSON.parse(row);
    
    							symbol = myObject.symbol;
    							price = myObject.price;
    							volume = myObject.volume;
    
    							if (matchRecogniseFirst == true){

								// This path is taken the first time that the row function runs
    
    								if (price > 0.50){
    									return [symbol, price, volume];
    								}
     
    								else{
    									return [];
    								}
    							}
    
    							else{
    		
    								var prevObject = JSON.parse(otherRow);								
    
    								if (volume > prevObject.volume){
    									return true;
    								}
    
    								else{
    									return false;
    								}
    							}
    							
    						}">>.
						
           	  
##Reduce Functions

Reduce Functions are called to aggregate results when a CEP Window Fires. They can be written in javascript or as pure erlang functions.

###Javascript Reduce Functions

A Reduce Function takes one parameter which is a multidimensional array containing the results from all the Row Functions that have successfully matched 
(i.e the arrays returned from all successfully matching row functions).

In this javascript example the matches parameter holds the following array :-

    [["Goog", 2.00, 20], ["Goog", 2.01, 21]]

    <<"var reduceFunction = function(matches){
	    					var sum = 0;
     
    						for(var i=0; i<matches.length; i++) {
    							var match = matches[i];
    							sum += match[2];
    						}
    
   						if (sum > 0){
   							return sum / matches.length;
    						}
    
    						return 0}">>.


##The feed_api

The feed_api is used to :-

1. create new feeds and windows.
2. Subscribe your application to be notified once Reduce Functions run.
3. Set-up joins.

The important functions are :-

1. feed_api:start_feed(<feed_name_atom>). - Starts a feed gen_server named after the atom.
2. feed_api:start_window(<window_name_atom>, <feed_name_atom>, RowFunction, ReduceFunction, QueryParameters, YourParameters). - Adds a window to the feed.  Please note that if you want to implement the Row and Reduce functions in javascript 
then the Row and Reduce Functions should be binaries containing javascript code.  If the Row and Reduce functions are implemented in erlang then these parameters should contain tuples containing the {ModuleName, FunctionName} of the Row and Reduce Functions.
3. feed_api:subscribe_feed_window(<feed_name_atom>, <window_name_atom>, Pid). - Subscribes a Pid to the feed / window.  This Pid is called when a match is found with the results from the Reduce Function. 

The parameters of start_window are :-

1. The name of the window as an atom.
2. The name for the feed as an atom.
3. A binary containing a javascript Row Function or a tuple containing a Module and Function Name {RowFunctionModule, RowFunctionName}.
4. A binary containing a javascript Reduce Function or a tuple containing a Module and Function Name {ReduceFunctionModule, ReduceFunctionName}
5. A list of configuration parameters.  View the Window Configuration section for more details.
6. A list of custom parameters that you can pass into the match function to trigger matches.  This could be a list of values that the Row Function must match as part of a complex pattern.   

The following code shows how to set-up a new feed, add a new window and subscribe a process to receive updates using javascript.

    %% Create a list containing some Binary Json.  [<<" valid json ">>, <<" Some more ">>]  
    Data = window_api:create_json([{"1.01", "14"}, {"1.02", "15"}, {"2.00", "16"}]),
    
    %% Generate a test Row Function - In practice this could come from a database it just has to be valid javascript within an Erlang binary.
    RowFunction = window_api:create_match_recognise_row_function(),
    	
    %% Generate a test Reduce Function - Again this could come from a database.
    ReduceFunction = window_api:create_reduce_function(),
    	
    %% Set-up a four second time based window looking for three consecutive matchRecognise rows.
    QueryParameterList = [{numberOfMatches, 3}, {windowSize, 4}, {matchType, matchRecognise}, {windowType, time}],
    
    %% Register a new feed called stress
    feed_api:start_feed(stress),
    	
    %% Create a new window called stressWin and apply it with out functions and parameters to the stress feed.
    feed_api:start_window(stressWin, stress, RowFunction, ReduceFunction, QueryParameterList, []),
    
    %% Spawn a callback function that will be messaged when the reduce function runs.
    Pid = spawn(?MODULE, get_response, []),
    
    %% Subscribe the callback function to the stressWin window.
    feed_api:subscribe_feed_window(stress, stressWin, Pid),
    
    lists:foreach(fun (Data) ->
    				%% Add data to the stress feed
    				feed_api:add_data(stress, DataElement)
    			  end, Data).

The following code shows an example subscriber call back function.

    get_response() ->
	    receive
		    Results ->
			    io:format("Stress test fired ~p ~n", [Results]),
			    get_response()
    	end.

##Row and Reduce Functions in erlang

Version 0.2 includes support for writing Row and Reduce functions in pure erlang.

A good example of a MatchRecognise query written in erlang can be found within example_erlang_match_recognise.erl.

When using erlang make sure that the Row and Reduce Function parameters within the feed_api:start_window function are tuples containing the module and function where the Row and Reduce Queries are implemented.

Theoretically you should be able to match erlang and javascript Row and Reduce functions but this has not been tested!

You can find a good example of an erlang matchRecognise rul09/05/2013 - e in feed_api:erlang_match_recognise_test() and the example_erlang_match_recognise module.
    	
##Searching

You can search within a window by calling 

    search_api:search_window(FeedName, WindowName, SearchParameter, Row, rowType).

Where :-

1. FeedName is the name of the feed;
2. WindowName is the name of the window;
3. SearchName is the search parameters in the format of a list.  Where the length of the list has to be the same size as the list returned from your row function.
4. Row is the latest input data.
5. rowType is an atom.  Set this to hardCoded if you don't want to perform any substitution
			Set to json if you want to perform json variable substitiution.

For example :-

['_', '_', '_'] - List all of the results in the window.

["GOOG", '_', '_'] - List all of the results where the first value returned from the row function is "GOOG".

["GOOG", 1.01, 12] - Lists all of the results where the stock is GOOG, the price is 1.01 and the volume is 12.	

["Obj.symbol", '_', '_'] - Extracts Obj.symbol from the latest row if the rowType atom is set to json

##Some more examples

Please look in stress_test.erl (run_match_test() and run_every_test()) and examples.erl for more examples.

There are many good examples in the eunit tests within feed_api.erl.

##Joining windows

You can join windows by making use of the the search api.

Joins need to be formatted as a list and added to a window using the feed_api:add_searches() function.

For example :-

		Search = [[joinFeedOne, joinWinOne, ['_', <<"Dan">>], hardCoded]],
		feed_api:add_searches(joinFeedOne, joinWinTwo, Search). 

This code sets up a hard coded search (i.e. one where search parameters do not need to be substituted) such that
every time new data is added to the joinWinTwo window in the joinFeedOne feed, the search query is run on the joinWinOne
window looking for the string Dan in the second results field.

The results of this search are applied to the join parameter within the Row Function in the format [ [FeedName, WindowName, [ Joined Rows ] ] ].

More complex searches can run over multiple windows. The results for a more complex multi-window search might look like this.

	[ [FeedName1, WindowName1, [ FeedName1 WindowName1 Joined Rows ] ], [FeedName2, WindowName2, [ FeedName2 WindowName2 Joined Rows ] ] ].

##Examples

See examples.erl.

##Dependencies

1. Spider Monkey
2. erlang_js
3. lager - An erlang logging framework

##Compilation / run Instructions

chmod +x rebar

Get the dependencies

    make deps

Clean the build environment

    make clean

Build and deploy release

    make deploy

Test

    make test

Start the application

    make run

Make sure that you run make deploy before running make test!

You can speed things up a bit by disabling lager logging within the sys.config file within /erlang_cep/rel/files.

##Logging / working out what has gone wrong.

I have included the excellent lager logging framework from Basho.  You can change the logging levels by modifying the sys.config settings within the rel/files directory. By default the logs are outputted to the 
/erlang_cep/rel/erlang_cep/log directory.

##TODO

Please note that this is a very early beta release, and I'm quite new to erlang..........

1. Implement every queries for standard windows, currently this functionality is only available for timed windows.
2. Look at failure scenarios.  Subscriptions and some config will not survive window process failures.
3. Implement more advanced CEP algorithms such as Rete.
4. Use Mnesia to try to make windows HA.
5. Write a web based UI to allow users to create feeds and view results.
6. Test, test and test again
7. Some more stress tests.
8. My MatchRecognise queries are very basic.  Add more functionality.
9. Use Lexx and Yacc to create a DSL to decsribe Windows and patterns. 
10. Each window currently has its own Javascript VM.  As the number of VM's increases this could cause issues.
11. Enhance the API so that users can write Row and Reduce functions in erlang.  Make sure that these functions are dynamically loaded.
12. Create a REST API through which users can add / mutate windows, and row / reduce functions.

Please let me know if you find any bugs!

##Licence

This software is open source.  It is licensed under the Apache 2 license.  Please feel free to email me at danmacklin10 at gmail.com if you have any issues or suggestions.

http://www.apache.org/licenses/LICENSE-2.0

##Change Log

19/05/2013 - V0.2 Allow Row and Reduce Functions to be written in pure erlang as well as Javascript