%%%-------------------------------------------------------------------
%%% @author Jakub Kalinski
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. mar 2025 18:51
%%%-------------------------------------------------------------------
-module(power).
-author("Jakub Kalinski").

%% API
-export([power/2]).

power(0, 0) -> none;
power(_, 0) -> 1;
power(0, _) -> 0;
power(Base, Exp) when Exp > 0 -> Base * power(Base, Exp-1);
power(Base, Exp) when Exp < 0 -> 1 / power(Base, -Exp).