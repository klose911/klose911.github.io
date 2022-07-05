-module(atom).
-export([convert/2, convert_length/1]).

convert(M, inch) ->
    M / 2.54;
convert(N, centimeter) ->
    N * 2.54.

convert_length({centimeter, X}) ->
    {inch, X / 2.54};
convert_length({inch, Y}) ->
    {centimeter, Y * 2.54}.
