%%%-------------------------------------------------------------------
%%% @author Jakub Kalinski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. kwi 2025 22:19
%%%-------------------------------------------------------------------
-module(anonymous_functions).
-author("Jakub Kalinski").

%% API
-export([sample_data/0, replace_chars/1, count_divisible_by_three1/1, count_divisible_by_three2/1, average_measurement/2]).

sample_data() ->
    [
        {"Station A", {2025,3,16}, {12,30}, [{pm10, 50.5}, {pm25, 30}, {temp, 15.2}]},
        {"Station B", {2025,3,16}, {13,00}, [{pm10, 40}, {pm25, 20.1}, {humidity, 60}]},
        {"Station A", {2025,3,16}, {14,00}, [{pm10, 55.3}, {pm25, 35}, {temp, 16.8}]},
        {"Station C", {2025,3,16}, {15,00}, [{pm10, 35}, {pm25, 18.7}, {pressure, 1015}]},
        {"Station A", {2025,3,17}, {10,00}, [{pm10, 60.2}, {pm25, 40}, {temp, 17.5}]},
        {"Station B", {2025,3,17}, {11,30}, [{pm10, 45}, {pm25, 25.6}, {humidity, 55}]},
        {"Station C", {2025,3,17}, {12,00}, [{pm10, 38.9}, {pm25, 22}, {pressure, 1012.4}]},
        {"Station A", {2025,3,18}, {09,00}, [{pm10, 65}, {pm25, 42.3}, {temp, 18.1}]},
        {"Station B", {2025,3,18}, {10,30}, [{pm10, 50}, {pm25, 28.4}, {humidity, 58}]},
        {"Station C", {2025,3,18}, {11,00}, [{pm10, 42.1}, {pm25, 24}, {pressure, 1010.5}]},
        {"Station D", {2025,3,19}, {08,45}, [{pm10, 48.7}, {pm25, 27.5}, {temp, 14.9}, {humidity, 59.8}]},
        {"Station D", {2025,3,19}, {09,45}, [{pm10, 52.3}, {pm25, 29.1}, {temp, 15.3}, {humidity, 60.2}]},
        {"Station A", {2025,3,19}, {10,30}, [{pm10, 61.5}, {pm25, 38.2}, {pressure, 1013.2}]}
    ].

replace_chars(Str) ->
    lists:map(fun($o) -> ($e);
                 ($e) -> ($o);
                 (X) -> X
        end, Str
    ).

count_divisible_by_three1(List) ->
    length(lists:filter(fun(X) ->
        X rem 3 == 0
                        end, List)).

count_divisible_by_three2(List) ->
    lists:foldl(fun(X, Acc) ->
        if X rem 3 == 0 -> Acc + 1;
            true -> Acc
        end
                end, 0, List).

average_measurement(Type, Data) ->
    ExtractedReadings = lists:map(fun({_, _, _, Readings}) -> Readings end, Data),
    Flattened = lists:foldl(fun(X, Acc) -> Acc ++ X end, [], ExtractedReadings),
    FilteredValues = lists:filter(fun({T, _}) -> T == Type end, Flattened),
    Values = lists:map(fun({_, Value}) -> Value end, FilteredValues),
    %% FilteredValues = [Value || {T, Value} <- Flattened, T == Type],
    case Values of
        [] -> 0;
        _ -> lists:sum(Values) / length(Values)
    end.