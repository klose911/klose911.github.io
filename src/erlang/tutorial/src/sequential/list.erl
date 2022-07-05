-module(list).
-export([list_length/1]).

list_length([]) ->
    0;    
list_length([First | Rest]) ->
    1 + list_length(Rest).
