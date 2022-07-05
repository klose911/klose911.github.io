-module(match).
-export([list_max/1]).

list_max([Head|Rest]) ->
    list_max(Rest, Head).

list_max([], Res) ->
    Res;
list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    New_result_far = Head, % more readable by user variable 
    list_max(Rest, New_result_far);
list_max([Head|Rest], Result_so_far)  ->
    list_max(Rest, Result_so_far).
