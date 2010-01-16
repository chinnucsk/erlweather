%% @author Zach Peters
%% @copyright  2010 The Helpful Hacker
%% @doc this is where my doc goes
%% @version 0.0.1
-module(weather).
-export([boot/0, start/0, stop/0, update/0, current/0, updateLoop/2]).

%% Include xmerl xml parsing library (your path may vary)
-include("/usr/lib/erlang/lib/xmerl-1.2/include/xmerl.hrl").

%% See http://www.nws.noaa.gov/tg/siteloc.shtml for station ids
-define(STATION, "KCWI").
-define(BASE_URL, "http://www.weather.gov/xml/current_obs/").


%% @spec weather_url_for(StationIdentifier) -> Url
%%    StationIdentifier = term()
%%    Url = term()
%% @doc returns url (from weather.gov) string for a given StationIdentifier
weather_url_for(StationIdentifier) ->
    ?BASE_URL ++ StationIdentifier ++ ".xml".

fetch_weather_for(StationIdentifier) ->    
    URL = weather_url_for(StationIdentifier),
    {ok, {{"HTTP/1.1", 200, "OK"}, _Headers, Body }} = http:request(URL),
    Body.
    
parse_weather_data(WeatherData) ->
    { Xml, _Rest } = xmerl_scan:string(WeatherData),
    [ #xmlText{value=TempF} ] = xmerl_xpath:string("//current_observation/temp_f/text()", Xml),
    {ok, TempF}.


updateLoop(Station, Data) ->    
    receive
	update ->
	    io:format("Updating readings...~n"),
	    {ok, TempF} = parse_weather_data(fetch_weather_for(Station)),
	    updateLoop(Station, TempF);
	current ->
	    io:format("Current readings: [~p] ~p~n", [Station, Data]),
	    updateLoop(Station, Data);
	stop ->
	    io:format("Stopping...~n");
	Any ->
	    io:format("Got unknown signal '~p'~n", [Any]),
	    updateLoop(Station, Data)
    end.


start() ->
    io:format("Starting weather server...~n"),
    inets:start(),
    Pid = spawn(weather, updateLoop, [?STATION, nil]),
    register(weather, Pid),
    weather ! update.

stop() ->
    weather ! stop,
    unregister(weather).

update() ->
    weather ! update.

current() ->
    weather ! current.

boot() ->
    weather:start(),
    weather:update(),
    timer:sleep(100),
    weather:current(),
    timer:sleep(100),
    weather:stop().
