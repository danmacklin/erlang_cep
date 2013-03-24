#erlang_cep v0.1 Beta 22/03/2013

A simple CEP (Complex Event Processing) engine written in erlang OTP inspired by esper and CouchDB. 

Erlang CEP facilitates the creation of one or many feeds, each with one or many time or size based windows.

Each window can be programmed to look for patterns within it's data.  Once a pattern is found the data is aggregated and
distributed to other processes via publish and subscribe.

Unlike many CEP systems which have their own custom DSL's erlang_cep can be programmed with two javascript functions (inspired by couchDB).  

The system is designed to offer highly concurrent processing as each feed and window run in seperate gen_server processes.

##How Does it Work?

The feed_api can be used to create Feeds and Windows.  Each feed can have one or many windows.

A Feed is a stream of json encoded data (other formats are possible).

A Window is a data storage abstraction with an embedded CEP rule that is applied to a Feed.

Each Window implements a Row and Reduce Function in javascript.  

The Row Function makes sure that only data which matches the CEP rule is added to the window.  

The Reduce Function is used to aggregate matched rows.

A good example is a Stock Market Feed where the Row Function would match stock data for a particular symbol.  
The Window configuration (with some help from the Row FUnction) would apply a CEP rule.  The Reduce 
Function would fire and aggregate the data if the Windows CEP rule matched.  Finally a publish and subscribe mechanism
would distribute the data to interested processes.

##Query Types

erlang_cep supports three types of query.

* Standard - Used to perform simple matches.  I.e. match every google stock with a sell price greater than £10.
           
* matchRecognise - Used to perform matches that make a progression.  I.e. match every google stock with a sell price greater than £10, where the next sale price is greater than the previous sell price.
				 
* every - Used to perform standard matches where the results are output at a regular interval.  I.e in a 60 second window give me the average sale price for all Google trades.

##Window Types

erlang_cep supports two types of window.

* size - Size based windows have a fixed size.  Each time data is added the position is incremented. Once the position equals the window size the position resets to 0 and the data within the window is overwritten and expired.  For example a size based window of five elements would facilitate the storage and query of five data points. 
       
* time - Time based windows expire data after a predetermined period of time.  For example a 60 second timed window would facilitate the storage and query of any number of data points added within a 60 second period of time.
       
##Window Configuration

Windows are configured through a list of tuples.  Version 0.1 supports the following configuration parameters.

The first element in the tuple is the parameter name, the second is a value or atom.

* {NumberOfMatches, Numeric} 	- The number of matches required to fire the Reduce Function.
* {WindowSize, Numeric} 	- The size of the window (number of elements for size based windows, time in seconds for time based windows), 
* {WindowType , size / time} 	- The type of the window size or time. 
* {Consecutive, consecutive / nonConsecutive} - Specifies whether matches need to be made up from consecutive or nonConsecutive data elements.  For example if this flag is set to consecutive, and NumberOfMatches is set to 3, then the reduce function will fire if the Row Function for three consecutive elements match			  
* {matchType, standard / matchRecognise / every}
* {resetStrategy, restart / noRestart}	- After the Reduce Function runs should the window be reset or carry on.
* {delete, yes / no} - After the Reduce Function runs should matched data be removed from the window.

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

Row Functions are defined as erlang binaries.  There are two types of Row Function standard and matchRecognise.

###A Standard Row Function

This is an example of a standard Row Function which will match if the price is greater than 1.00.

    <<"var rowFunction = function(parameters, joins, row, otherRow, first){
    							
    							var myObject = JSON.parse(row);
    							symbol = myObject.symbol;
    							price = myObject.price;
    							volume = myObject.volume;
    
    							if (price > 1.00){
    								return [symbol, price, volume];
    							}
    							
    							return []}">>.
							
N.B to debug a javascript function add ejsLog("/tmp/foo.txt", "Hello"); to your javascript.


The Row Function takes in five parameters :-

1. parameters - A list of parameters that are passed into the rowFunction.  Can be useful for when you want to pass some filters into your row function i.e. if (price > parameters[0]) rather than if (price > 1.00)
2. joins - A list of joined data where each row in the list represents a join.  [ [FeedName, WindowName, [ Joined Rows ]] ].  Each joined row is a list of one or more elements.
3. row - A String holding the new json document to be matched (I refer to this as a Row).
4. otherRow - A string holding the json data from the last / previous match.
5. first - A boolean value set to true if this is an initial row check, false if this is checking the new row against previous values.

_NB_ For standard windows only the row parameter is used.

The Row Function compiles the row String into a json object and then extracts the data. If the price is greater than 1.00 
the function is said to have matched and returns a list containing the data.  N.B the order of this list is important as
it is used within Reduce Functions to generate aggregate values if the window fires. 

If the Row Function fails to match it must return an empty list.

N.B I have started to write an API which makes searching for parameters within joins a little more straightforwards.

To use the API create a Join object passing in the join parameter of your row function.  var j = new Join(joins);

The API includes the following functions :-

1. exists(value, position) - Search the joins array looking for value within results position 0.  Remember that when a row function returns it returns an array of one or many values.  The position parameter looks for a value in the specified position.  
2. TODO - exists(value) - Search every position on the joins array for value.
3. TODO - exists(value, position, feedName, windowName)
4. TODo - exisits(value,feedName, windowName)

Here is an example.

	var rowFunction = function(parameters, joins, row, otherRow, first){
		var j = new Join(joins);

		if (j.exists(parameters[0], 0)){
			return [\"Yes\"];
		}
		
	return [\"Nope\"]}

It returns Yes if it finds the value of parameters[0] at position 0 it returns Yes.

###A MatchRecognise Row Function

MatchRecognise Functions are used to find complex patterns in data.  Typically they are called twice, once to check that new row data passes an initial
threshold, then again to check new row data against previous matches.

When first is set to true this function is being called within the context of an initial match check.

When first is set to false this function is being called within the context of checking the new data against previous values.  

    <<"var rowFunction = function(parameters, joins, row, otherRow, first){
    							
    							var myObject = JSON.parse(row);
    
    							symbol = myObject.symbol;
    							price = myObject.price;
    							volume = myObject.volume;
    
    							if (first == true){
    
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
						

The function takes in three parameters :-

1. parameters - A list of parameters that are passed into the rowFunction.  Can be useful for when you want to pass some filters into your row function i.e. if (price > parameters[0]) rather than if (price > 1.00)
2. joins - A list of joined data where each row in the list represents a join.  [ [FeedName, WindowName, [ Joined Rows ]] ]
3. row 	- A String holding the new json document.
4. otherRow - A string holding the json data from the last / previous match.
5. first - A boolean value set to true when this function is called from the context of an initial check, and false when called in the context of matchRecognise.
           	  
The initial check code (first = true) is very similar to the standard Row Function returning an array of values if the row matches, and an empty list if
it fails.

The matchRecognise code (after the else) compares the new json row to a previous json row and returns true if the new data passes the test and false otherwise.

##Reduce Functions

Reduce Functions are called to aggregate the results when a Window Fires.  For example if a window is set-up to look for three consecutive matches and three
valid rows match the Row Function then the window will fire it's Reduce Function.

The Reduce Function takes in one parameter which is a multidimensional array containing the results from all of the Row Functions that have successfully matched 
(i.e the arrays returned from all successfully matching row functions).

For example :-

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
							
The Reduce Function can then return any value back to the CEP system.  In this example the volume attribute (the 3rd element in arrays returned from the Row Functions)
is extracted and used to create an average volume.

##The feed_api

The feed_api is used to :-

1. create new feeds.
2. create new windows.
3. Subscribe your application to be notified when Reduce Functions run.
4. Search within windows.

The important functions are :-

1. feed_api:start_feed(<feed_name_atom>). - Starts a new feed gen_server named after the atom.
2. feed_api:start_window(<window_name_atom>, <feed_name_atom>, RowFunction, ReduceFunction, QueryParameters, YourParameters). - Adds a window to the feed.
3. feed_api:subscribe_feed_window(<feed_name_atom>, <window_name_atom>, Pid). - Subscribes a Pid to the feed / window.  This Pid is called when a match is found. 

The parameters of start_window are :-

1. The name of the window as an atom.
2. The name for the feed as an atom.
3. A binary containing the javascript row function.
4. A binary containing the javascript reduce function.
5. A list of configuration parameters.  View the Window Configuration section for more details.
6. A list of custom parameters that you can pass into the match function to tailer matches.   

The following code shows how to set-up a new feed, apply a new window and subscribe a process to updates.

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

The following code shows an example callback function.

    get_response() ->
	    receive
		    Results ->
			    io:format("Stress test fired ~p ~n", [Results]),
			    get_response()
    	end.
    	
##Searching

You can search within a window by calling search_api:search_window(FeedName, WindowName, SearchParameter, Row, rowType).

Where :-

1. FeedName is the name of the feed;
2. WindowName is the name of the window;
3. SearchName is the search parameters in the format of a list.  Where the length of the list has to be the same size as the list returned from your row function.
4. Row is the latest input row
5. rowType is an atom.  Set this to hardCoded if you don't want to perform any substitution
			Set to json if you want to perform json variable substitiution.

For example :-

['_', '_', '_'] - List all of the results in the window.

["GOOG", '_', '_'] - List all of the results where the first value returned from the row function is "GOOG".

["GOOG", 1.01, 12] - Lists all of the results where the stock is GOOG, the price is 1.01 and the volume is 12.	

["Obj.symbol", '_', '_'] - Extracts Obj.symbol from the latest row if the rowType atom is set to json

##Some more examples

Please look in stress_test.erl (run_match_test() and run_every_test()) for more examples.

There are also several good examples within the enit tests within feed_api.erl.

##Joining windows

You can join windows by using the search function.

Joins need to be formatted as a list and then added to a window using the feed_api:add_searches() function.

For example :-

		Search = [[joinFeedOne, joinWinOne, ['_', <<"Dan">>], hardCoded]],
		feed_api:add_searches(joinFeedOne, joinWinTwo, Search). 

This code sets up a hard coded search (i.e. one where search parameters do not need to be substituted) such that
every time new data is added to the joinWinTwo window in the joinFeedOne feed, a query is run on the joinWinOne
window looking for the string Dan in the second results field.

The results of this search are applied to the join parameter within the row function in the format [ [FeedName, WindowName, [ Joined Rows ]] ].

##Examples

See examples.erl.

##Dependencies

1. Spider Monkey
2. erlang_js
3. lager - An erlang logging framework

##Compilation / run Instructions

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

N.B Make sure that you run make deploy before running make test!

##Logging / working out what has gone wrong.

I have included the excellent lager logging framework from Basho.  You can change the logging levels by modifying
the sys.config settings within the rel/files directory. By default the logs are outputted to the 
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

##Licence

This software is open source.  It is licensed under the Apache 2 license.  Please feel free to email me at danmacklin10 at gmail.com if you have any issues or suggestions.

http://www.apache.org/licenses/LICENSE-2.0