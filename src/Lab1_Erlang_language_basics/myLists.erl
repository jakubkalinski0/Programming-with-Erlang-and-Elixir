%%%-------------------------------------------------------------------
%%% @author Jakub Kalinski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. mar 2025 19:14
%%%-------------------------------------------------------------------
-module(myLists).
-author("Jakub Kalinski").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1, sumFloatsTail/1]).

contains(_, []) -> false;
contains(X, [X | _]) -> true;
contains(X, [_ | T]) -> contains(X, T).

duplicateElements([]) -> [];
duplicateElements([H | T]) -> [H, H | duplicateElements(T)].

sumFloats(List) ->
    lists:sum([X || X <- List, is_float(X)]).

sumFloatsTail(List) -> sumFloatsTail(List, 0).
sumFloatsTail([], Sum) -> Sum;
sumFloatsTail([H | T], Sum) when is_float(H) -> sumFloatsTail(T, Sum + H);
sumFloatsTail([_ | T], Sum) -> sumFloatsTail(T, Sum).