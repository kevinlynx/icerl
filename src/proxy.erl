%%
%% proxy.erl
%% Kevin Lynx
%% 05.07.2013
%%
-module(proxy).
-include("msg.hrl").
-import(stream, [bjoin/1]).
-record(proxy, {objname, sock, req}).
-export([create/3, create/2, destroy/1, check_cast/2, invoke/3]).

create(ObjName, Ip, Port) ->
    case connect(Ip, Port) of
        {error, Reason} -> {error, Reason};
        {ok, Sock} -> {ok, #proxy{objname = ObjName, sock = Sock, req = 1}}
    end.

create(ObjName, Port) ->
    create(ObjName, "localhost", Port).

destroy(#proxy{sock = Sock} = _Proxy) ->
    send(Sock, msg:encode_close_msg()),
    gen_tcp:close(Sock).

connect(Ip, Port) ->
    case gen_tcp:connect(Ip, Port, [binary, {active, once}]) of
        {ok, Sock} ->
            receive 
                {tcp, Sock, Data} ->
                    case handle_connect(Data, Sock) of
                        true -> {ok, Sock};
                        false -> {error, invalid_msg}
                    end
            after
                1000 -> {error, invalid_connection}
            end;
        _ ->
            {error, connect_failed}
    end.

send(Sock, Data) ->
    gen_tcp:send(Sock, Data).

recv(Sock) ->
    gen_tcp:recv(Sock, 0).

handle_connect(Data, _Sock) ->
    {Header, _} = msg:decode_header(Data),
    Header#header.msgtype == ?MSG_VALIDATE_CONN.
    
check_cast(Proxy, Type) ->
    Arg = stream:write(Type),
    invoke(Proxy, "ice_isA", [Arg]).

invoke(Proxy, Method, Args) when is_list(Args) ->
    ArgB = bjoin(Args),
    Req = make_request(Proxy#proxy.objname, Method, Proxy#proxy.req, ArgB),
    send(Proxy#proxy.sock, msg:encode_request_msg(Req)),
    case recv(Proxy#proxy.sock) of
        {ok, ResB} ->
            Reply = msg:decode_reply(split_header(ResB, ?MSG_REPLY)),
            Ret = case Reply#reply.req == Proxy#proxy.req of
                true -> Reply#reply.encap#encap.payload;
                false -> <<>>
            end,
            {ok, Proxy#proxy{req = Proxy#proxy.req + 1}, Ret};
        _ ->
            {error, Proxy, <<>>}
    end.

make_request(ObjName, Method, ReqNo, ArgB) ->
    #request{
        req = ReqNo,
        id = #identity{name = ObjName},
        operation = Method,
        encap = #encap{payload = ArgB}}.

split_header(Reply, Type) ->
    {H, Body} = msg:decode_header(Reply),
    if 
        H#header.msgtype /= Type -> <<>>;
        true -> Body end.
    
