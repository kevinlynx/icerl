%%
%% conn.erl
%% Kevin Lynx
%% 05.06.2013
%%
-module(conn).
-include("msg.hrl").
-compile(export_all).
-export([handle_accept/1, handle_data/3]).
-import(stream, [bjoin/1]).

handle_accept(Sock) ->
    B = msg:encode_validate_conn_msg(),
    gen_tcp:send(Sock, B).

handle_data(Adapter, Sock, Pack) ->
    {H, Body} = msg:decode_header(Pack),
    handle_request(Adapter, H#header.msgtype, Sock, Body).

handle_request(Adapter, ?MSG_REQUEST, Sock, Body) ->
    {Req, _} = msg:decode_request(Body),
    process_request(Adapter, Req, Sock);

handle_request(_Adapter, ?MSG_CLOSE, Sock, _) ->
    gen_tcp:send(Sock, msg:encode_close_msg()).

process_request(Adapter, Req, Sock) ->
    ObjName = Req#request.id#identity.name,
    MName = Req#request.operation,
    Arg = Req#request.encap#encap.payload,
    {Status, Ret} = case adapter:invoke(Adapter, ObjName, MName, Arg, Sock) of
        {?REPLY_SUCCESS, R} -> {?REPLY_SUCCESS, R};
        Code -> {Code, []}
    end,
    ResB = bjoin(Ret),
    Reply = #reply{req = Req#request.req, status = Status, encap = #encap{payload = ResB}},
    gen_tcp:send(Sock, msg:encode_reply_msg(Reply)).

