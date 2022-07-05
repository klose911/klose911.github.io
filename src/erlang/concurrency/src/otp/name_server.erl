%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(name_server).
-export([init/0, add/2, find/1, handle/2]).
-import(server2, [rpc/2]).

%% client routines
add(Key, Value) -> rpc(name_server, {add, Key, Value}).
find(Key)       -> rpc(name_server, {find, Key}).

%% callback routines
init() -> dict:new().
handle({add, Key, Value}, Dict) -> {ok, dict:store(Key, Value, Dict)};
handle({find, Key}, Dict)       -> {dict:find(Key, Dict), Dict}.

