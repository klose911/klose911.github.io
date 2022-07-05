-module(func). 
-export([double/1, fact/1, mult/2]).

double(X) ->
    2 * X.

fact(1) ->
    1;
fact(N) ->
    N * fact(N - 1).

mult(X, Y) ->
    X * Y.
