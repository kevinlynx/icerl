%%
%% testadapter.erl
%% Kevin Lynx
%% 05.06.2013
%% communicate with Ice/minimal demo
%%
-module(testadapter).
-compile(export_all).
-export([start/0]).

ice_isA(Arg) ->
    {T, _} = stream:read(string, Arg),
    io:format("ice_isA: ~s~n", [T]),
    [stream:write(true)].

say_hello(_Arg) ->
    io:format("hello world~n"),
    [].

start() ->
    Objects = [{"hello", [{"ice_isA", fun ? MODULE:ice_isA/1}, 
                    {"sayHello", fun ?MODULE:say_hello/1}]}],
    adapter:create("Hello", 10000, Objects).

