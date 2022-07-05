%% ---
%%  Excerpted from "Programming Erlang, Second Edition",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
%%---
-module(server1).
-export([start/2, rpc/2]).


start(ProcessName, Mod) ->
    register(ProcessName, spawn(fun() -> loop(ProcessName, Mod, Mod:init()) end)).

rpc(ProcessName, Request) ->
    ProcessName ! {self(), Request},
    receive
        {ProcessName, Response} -> Response
    end.

loop(ProcessName, Mod, State) ->
    receive
	{FromProcess, Request} ->
            {Response, State1} = Mod:handle(Request, State),
            FromProcess ! {ProcessName, Response},
            loop(ProcessName, Mod, State1)
    end.
