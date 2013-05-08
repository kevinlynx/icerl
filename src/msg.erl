%%
%% msg.erl
%% Kevin Lynx
%% 04.20.2013
%%
-module(msg).
-include("msg.hrl").
-export([encode_request/1, encode_validate_conn_msg/0, encode_close_msg/0,
        encode_request_msg/1, encode_reply_msg/1]).
-import(stream, [bjoin/1]).
-compile(export_all).

decode_identify(B) ->
    {Name, B1} = stream:read(string, B),
    {Cate, B2} = stream:read(string, B1),
    {#identity{name = Name, cate = Cate}, B2}.

decode_header(B) ->
    <<I:8, C:8, E:8, P:8, ProtoMajor:8, ProtoMinor:8, EncodeMajor:8, EncodeMinor:8,
        Msg:8, ComS:8, Size:32/little-integer, Body/binary>> = B,
    {#header{magic = [I, C, E, P], protoMajor = ProtoMajor, protoMinor = ProtoMinor,
        encodeMajor = EncodeMajor, encodeMinor = EncodeMinor,
        msgtype = Msg, comStatus = ComS, msgsize = Size}, Body}.

encode_context(D) ->
    stream:write_dict(D).

decode_context(B) ->
    stream:read_dict(B, string, string).

encode_identify(I) ->
    bjoin([stream:write(I#identity.name), stream:write(I#identity.cate)]).

encode_facet(F) ->
    stream:write_seq(F).

% encode a `request' to binary
encode_request(R) ->
    bjoin([stream:write(int, R#request.req),
            encode_identify(R#request.id),
            encode_facet(R#request.facet),
            stream:write(R#request.operation),
            stream:write(byte, R#request.mode),
            encode_context(R#request.context),
            stream:write_encap(R#request.encap#encap.payload)]).

decode_request(B) ->
    {Req, B1} = stream:read(int, B),
    {Id, B2} = decode_identify(B1),
    {Facet, B3} = stream:read_seq(B2, string),
    {Op, B4} = stream:read(string, B3),
    {Mode, B5} = stream:read(byte, B4),
    {Context, B6} = decode_context(B5),
    {{Size, Major, Minor, Payload}, Rest} = stream:read_encap(B6),
    {#request{req = Req,
       id = Id, facet = Facet, operation = Op, 
       mode = Mode, context = Context,
       encap = #encap{size = Size, major = Major, minor = Minor, payload = Payload}}, Rest}.

% encode a `header` to binary
encode_header(#header{msgtype=T, comStatus=C, msgsize=S} = _H) ->
    <<"IceP", 1, 0, 1, 0, T:8, C:8, S:32/little>>.

% decode a reply record.
decode_reply(B) ->
    <<Req:32/little, Status:8, Rest/binary>> = B,
    {{Size, Major, Minor, Payload}, _} = stream:read_encap(Rest),
    #reply{req = Req, status = Status, encap = 
        #encap{size = Size, major = Major, minor = Minor, payload = Payload}}.

encode_reply(R) ->
    #reply{req = Req, status = Status, 
        encap = #encap{major = Major, minor = Minor, payload = Payload}} = R,
    bjoin([<<Req:32/little, Status:8>>, stream:write_encap(Payload, {Major, Minor})]).

encode_validate_conn_msg() ->
    encode_header(#header{msgtype = ?MSG_VALIDATE_CONN}).

encode_close_msg() ->
    H = #header{msgtype = ?MSG_CLOSE, comStatus = 0, msgsize = 14},
    encode_header(H).

encode_reply_msg(R) ->
    Body = encode_reply(R),
    H = #header{msgtype = ?MSG_REPLY, msgsize = byte_size(Body) + 14},
    HB = msg:encode_header(H),
    bjoin([HB, Body]).

encode_request_msg(R) ->
    RB = msg:encode_request(R),
    H = #header{msgtype = ?MSG_REQUEST, msgsize = byte_size(RB) + 14},
    HB = msg:encode_header(H),
    bjoin([HB, RB]).

