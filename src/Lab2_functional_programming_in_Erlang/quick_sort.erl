%%%-------------------------------------------------------------------
%%% @author Jakub Kalinski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2025 13:32
%%%-------------------------------------------------------------------
-module(quick_sort).
-author("Jakub Kalinski").

%% API
-export([less_than/2, grt_eq_than/2, qs/1, random_elems/3, compare_speeds/3]).

less_than([], _) -> [];
less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than([], _) -> [];
grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([]) -> [];
qs([Pivot | Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs(grt_eq_than(Tail,Pivot)).

random_elems(_, Min, Max) when Min > Max -> error;
random_elems(N, _, _) when N < 0 -> error;
random_elems(0, _, _) -> error;
random_elems(N, Min, Max) -> [rand:uniform(Max-Min+1) + Min - 1 || _ <- lists:seq(1, N)].

compare_speeds([], _, _) -> none;
compare_speeds(List, Fun1, Fun2) ->
    {Time1, _} = timer:tc(Fun1, [List]),
    {Time2, _} = timer:tc(Fun2, [List]),
    io:format("Algorithm 1 took ~p microseconds.~n", [Time1]),
    io:format("Algorithm 2 took ~p microseconds.~n", [Time2]),
    case Time1 < Time2 of
        true -> io:format("Algorithm 1 is faster.~n");
        false -> io:format("Algorithm 2 is faster or they are equal.~n")
    end.