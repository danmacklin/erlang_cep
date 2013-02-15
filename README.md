#erlang_cep v0.1 Beta 30/01/2013

A Complex Event Processing (CEP) engine written in erlang that makes use of javascript to
simplify the creation of CEP rules.  Designed to run on Linux / Unix systems.

##Dependencies

1. Spider Monkey
2. erlang_js

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

##How Does it Work?

The erlang_cep feed_api facilitates the creation of Feeds and Windows where a Feed can
have zero or many Windows. 

A Feed is a stream of json encoded data (other formats should be possible).

A Window is a CEP rule that is applied to a Feed.

Each Window implements a Row and Reduce Function in javascript.  

The Row Function is used to match rows.  The Reduce Function is used to aggregate matched rows.

A good example would be a Stock Market Data Feed, where a Row Function would be used to match stock for
a particular symbol, price and volume.  The Reduce Function might then be used to calculate
the average sale price of all matched items.

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

* {NumberOfMatches, Numeric} - The number of matches required to fire the Reduce Function.
* {WindowSize, Numeric} - The size of the window (number of elements for size based windows, time in seconds for time based windows), 
* {WindowType , size / time} - The type of the window size or time. 
* {Consecutive, consecutive / nonConsecutive} - Specifies whether matches need to be made up from consecutive or nonConsecutive data elements.  For example if this flag is set to consecutive, and NumberOfMatches is set to 3, then the reduce function will fire if the Row Function for three consecutive elements match			  
* {matchType, standard / matchRecognise / every}
* {resetStrategy, restart / noRestart}		- After the Reduce Function runs should the window be reset or carry on.

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

    <<"var rowFunction = function(row, otherRow, first){
    							
    							var myObject = JSON.parse(row);
    							symbol = myObject.symbol;
    							price = myObject.price;
    							volume = myObject.volume;
    
    							if (price > 1.00){
    								return [symbol, price, volume];
    							}
    							
    							return []}">>.
							
The Row Function takes in three parameters :-

1. row - A String holding the new json document to be matched (I refer to this as a Row).
2. otherRow - A string holding the json data from the last / previous match.
3. first - A boolean value set to true if this is an initial row check, false if this is checking the new row against previous values.

_NB_ For standard windows only the row parameter is used.

The Row Function compiles the row String into a json object and then extracts the data. If the price is greater than 1.00 
the function is said to have matched and returns a list containing the data.  N.B the order of this list is important as
it is used within Reduce Functions to generate aggregate values if the window fires. 

If the Row Function fails to match it must return an empty list.

###A MatchRecognise Row Function

Next is an example of a matchRecognise Row Function.  MatchRecognise Functions can be called twice, once to check that new row data passes an initial
match, then again to check new row data against previous matches.

When first is set to true this function is being called within the context of an initial match check.

When first is set to false this function is being called within the context of checking the new data against previous values.  

    <<"var rowFunction = function(row, otherRow, first){
    							
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

1. row 		- A String holding the new json document.
2. otherRow - A string holding the json data from the last / previous match.
3. first 	- A boolean value set to true when this function is called from the context of an initial check, and false when called in the context of matchRecognise.
           	  
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
    feed_api:start_window(stressWin, stress, RowFunction, ReduceFunction, QueryParameterList),
    
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

You can search within a window by calling feed_api:search_window(FeedName, WindowName, SearchParameter).

Where :-

1. FeedName is the name of the feed;
2. WindowName is the name of the window;
3. SearchName is the search parameters in the format of a list.  Where the length of the list has to be the same size as the list returned from your row function.

For example :-

['_', '_', '_'] - List all of the results in the window.

["GOOG", '_', '_'] - List all of the results where the first value returned from the row function is "GOOG".

["GOOG", 1.01, 12] - Lists all of the results where the stock is GOOG, the price is 1.01 and the volume is 12.	

##Some more examples

Please look in stress_test.erl (run_match_test() and run_every_test()) for more examples.

There are also several good examples within the enit tests within feed_api.erl.

##TODO

Please note that this is a very early beta release, and I'm quite new to erlang..........

1. Implement every queries for standard windows.
2. Look at failure scenarios.  Currently subscriptions will not survive process failures.
3. Implement more advanced CEP algorithms such as Rete
4. Work out how to join windows.
5. Use Mnesia to try to make windows HA.
6. Write a web based UI to allow users to create feeds and view results.
7. Test, test and test again
8. Some more stress tests.
9. My MatchRecognise queries are very basic.  Add more functionality.

##Proposal for inter-window look-ups

It might be useful to write row functions that are able to pull values from other windows.

Lets say that we have a time based window containing the IPAddresses and usernames from which people have tried to hack
our systems within the last hour.  We call this window hackerWindow.

We now have a requirement to work out the value of all transactions from ip addresses that reside within
this hacker window.
			
create_lookup_standard_row_function() ->
	    <<"var rowFunction = function(row, otherRow, first, OtherWindowsData){
    							
    							var myObject = JSON.parse(row);
    							srcIp = myObject.srcIp;
    							url = myObject.url;
    							transactionValue = myObject.transactionValue
    							
    							var matched = false;
    							
    							exitLoop:
    							for (int x=0; x < otherWindowsData.length; x++)
    							{
    								for (int y=0; y< otherWindowsData[x][y].length; y++) 
    									if (otherWindowsData[x][y][0] == srcIp)
    									{
    										matched = true;
    										break exitLoop;
    									}
    								}
    							}
    
    							if (matched == true){
    								return [srcIp, url, transactionValue];
    							}
    							
    							return []}">>.

So how do we populate otherWindowMatches?

We call out to the feed genserver for a list of [{feedName, windowName}].  This returns [ [[win1res1,win1res2],[win1res1,win1res2]], [[win2res1, win2res2]] ].