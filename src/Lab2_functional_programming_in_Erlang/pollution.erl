%%%-------------------------------------------------------------------
%%% @author Jakub Kalinski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. kwi 2025 01:15
%%%-------------------------------------------------------------------
-module(pollution).
-author("Jakub Kalinski").

%% API
-export([
    create_monitor/0,
    add_station/3,
    add_value/5,
    remove_value/4,
    get_one_value/4,
    get_station_mean/3,
    get_daily_mean/3
]).

% Create a new monitor
create_monitor() ->
    #{
        stations => #{},  % Map of stations by name
        coords => #{},    % Map of station names by coordinates
        data => #{}       % Map of measurements
    }.

% Add a new station to the monitor
add_station(Name, Coords, Monitor) ->
    #{stations := Stations, coords := Coords_Map} = Monitor,

    % Check if station with this name already exists
    case maps:is_key(Name, Stations) of
        true ->
            {error, "Station with this name already exists"};
        false ->
            % Check if station with these coordinates already exists
            case maps:is_key(Coords, Coords_Map) of
                true ->
                    {error, "Station with these coordinates already exists"};
                false ->
                    % Add the new station
                    NewStations = maps:put(Name, Coords, Stations),
                    NewCoords = maps:put(Coords, Name, Coords_Map),
                    Monitor#{stations := NewStations, coords := NewCoords}
            end
    end.

% Helper function to find station by name or coordinates
find_station(NameOrCoords, Monitor) ->
    #{stations := Stations, coords := Coords_Map} = Monitor,

    case is_tuple(NameOrCoords) of
        true ->
            % NameOrCoords is coordinates
            case maps:find(NameOrCoords, Coords_Map) of
                {ok, StationName} -> {ok, StationName, maps:get(StationName, Stations)};
                error -> {error, "Station not found"}
            end;
        false ->
            % NameOrCoords is a name
            case maps:find(NameOrCoords, Stations) of
                {ok, StationCoords} -> {ok, NameOrCoords, StationCoords};
                error -> {error, "Station not found"}
            end
    end.

% Add a measurement value to a station
add_value(NameOrCoords, DateTime, Type, Value, Monitor) ->
    #{data := Data} = Monitor,

    % Find the station
    case find_station(NameOrCoords, Monitor) of
        {ok, StationName, StationCoords} ->
            % Create a key for the measurement
            MeasurementKey = {StationCoords, DateTime, Type},

            % Check if this measurement already exists
            case maps:is_key(MeasurementKey, Data) of
                true ->
                    {error, "Measurement already exists"};
                false ->
                    % Add the new measurement
                    NewData = maps:put(MeasurementKey, Value, Data),
                    Monitor#{data := NewData}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% Remove a measurement value from a station
remove_value(NameOrCoords, DateTime, Type, Monitor) ->
    #{data := Data} = Monitor,

    % Find the station
    case find_station(NameOrCoords, Monitor) of
        {ok, _StationName, StationCoords} ->
            % Create a key for the measurement to remove
            MeasurementKey = {StationCoords, DateTime, Type},

            % Check if the measurement exists
            case maps:is_key(MeasurementKey, Data) of
                true ->
                    % Remove the measurement
                    NewData = maps:remove(MeasurementKey, Data),
                    Monitor#{data := NewData};
                false ->
                    {error, "Measurement not found"}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% Get a single measurement value
get_one_value(NameOrCoords, DateTime, Type, Monitor) ->
    #{data := Data} = Monitor,

    % Find the station
    case find_station(NameOrCoords, Monitor) of
        {ok, _StationName, StationCoords} ->
            % Create a key for the measurement
            MeasurementKey = {StationCoords, DateTime, Type},

            % Get the measurement value
            case maps:find(MeasurementKey, Data) of
                {ok, Value} -> Value;
                error -> {error, "Measurement not found"}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% Calculate the mean value of a type for a specific station
get_station_mean(NameOrCoords, Type, Monitor) ->
    #{data := Data} = Monitor,

    % Find the station
    case find_station(NameOrCoords, Monitor) of
        {ok, _StationName, StationCoords} ->
            % Filter measurements for this station and type
            Values = [Value || {{Coords, _DateTime, MeasType}, Value} <- maps:to_list(Data),
                Coords =:= StationCoords, MeasType =:= Type],

            % Calculate mean value if there are measurements
            case Values of
                [] -> {error, "No measurements found for this station and type"};
                _ -> lists:sum(Values) / length(Values)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

% Calculate the daily mean value of a type across all stations
get_daily_mean(Type, Date, Monitor) ->
    #{data := Data} = Monitor,

    % Filter measurements for this date and type
    Values = [Value || {{_Coords, {DateVal, _Time}, MeasType}, Value} <- maps:to_list(Data),
        DateVal =:= Date, MeasType =:= Type],

    % Calculate mean value if there are measurements
    case Values of
        [] -> {error, "No measurements found for this date and type"};
        _ -> lists:sum(Values) / length(Values)
    end.