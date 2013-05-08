%% 
%% testproxy.erl
%% Kevin Lynx
%% 05.07.2013
%%
-module(testproxy).
-export([start/0]).

start() ->
    {ok, Proxy} = proxy:create("hello", 10000),
    {ok, HelloPrx, _Res} = proxy:check_cast(Proxy, "::Demo::Hello"),
    {ok, NewPrx, _} = proxy:invoke(HelloPrx, "sayHello", []),
    proxy:destroy(NewPrx).


