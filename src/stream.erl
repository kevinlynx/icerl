%%
%% stream.erl
%% basic data types encode/decode for Ice
%% Kevin Lynx
%% 04.19.2013
%%
-module(stream).
-export([bjoin/1, write/1, write/2, write_encap/2, write_encap/1, write_seq/1, write_dict/1,
    read/2, read_seq/2, read_dict/3, read_size/1]).
-compile(export_all).

bjoin(List) ->
    lists:foldr(fun(A, B) -> <<A/binary, B/binary>> end, <<>>, List).

write(V) when is_boolean(V) ->
    if V -> <<1>>; true -> <<0>> end;

write(V) when is_float(V) ->
    <<V:64/little-float>>;

write({T, V}) ->
    write(T, V);

write([]) ->
    write_size(0);

write(S) when is_list(S) ->
    bjoin([write_size(length(S)), list_to_binary(S)]).

write(byte, V) ->
    <<V:8>>;

write(short, V) ->
    <<V:16/little>>;

write(int, V) ->
    <<V:32/little>>;

write(long, V) ->
    <<V:64/little>>;

write(float, V) ->
    <<V:32/little-float>>.

write_size(S) ->
    if 
        S < 255 -> <<S>>;
        true -> <<255:8, S:32/little>> end.

write_encap(Payload, {Major, Minor}) ->
    Size = byte_size(Payload) + 6,
    bjoin([write(int, Size), write(byte, Major), write(byte, Minor),
            Payload]).

write_encap(Payload) ->
    write_encap(Payload, {1, 1}).

write_seq_content([H|T]) ->
    bjoin([write(H), write_seq_content(T)]);

write_seq_content([]) ->
    <<>>.

write_seq([]) ->
    write_size(0);

write_seq(S) ->
    bjoin([write_size(length(S)), write_seq_content(S)]).

write_dict_content([]) ->
    <<>>;

write_dict_content([{K, V}|T]) ->
    bjoin([write(K), write(V), write_dict_content(T)]).

write_dict([]) ->
    write_size(0);

write_dict(D) ->
    bjoin([write_size(length(D)), write_dict_content(D)]).

% read function set
read(bool, <<V:8, R/binary>>) ->
    {V /= 0, R};

read(byte, <<V:8, R/binary>>) ->
    {V, R};

read(short, <<V:16/little-integer, R/binary>>) ->
    {V, R};

read(int, <<V:32/little-integer, R/binary>>) ->
    {V, R};

read(long, <<V:64/little-integer, R/binary>>) ->
    {V, R};

read(float, <<V:32/little-float, R/binary>>) ->
    {V, R};

read(double, <<V:64/little-float, R/binary>>) ->
    {V, R};

read(string, B) ->
    {Size, B1} = read_size(B),
    if 
        Size == 0 -> {[], B1};
        true -> <<V:Size/binary, B2/binary>> = B1,
            {binary_to_list(V), B2} end;

read(_T, <<>>) ->
    null.

read_size(<<F:8, R/binary>>) ->
    if 
        F < 255 -> {F, R};
        true -> read(int, R) end.

read_encap(B) ->
    <<Size:32/little, Major:8, Minor:8, R/binary>> = B,
    PSize = Size - 6,
    <<Payload:PSize/binary, Rest/binary>> = R,
    {{Size, Major, Minor, Payload}, Rest}.

read_seq_content(_B, _T, 0) ->
    null;

read_seq_content(B, T, Cnt) ->
    {R, B1} = read(T, B),
    case read_seq_content(B1, T, Cnt - 1) of
        null ->
            {[R], B1};
        {R1, B2} ->
            {[R] ++ R1, B2}
    end.

% read sequence, the item type is `T'
read_seq(B, T) ->
    {Size, B1} = read_size(B),
    if 
        Size == 0 -> {[], B1};
        true ->
            read_seq_content(B1, T, Size)
    end.


% read dictcotry, the pair type is <KT, VT>
read_dict(B, KT, VT) ->
    {Size, B1} = read_size(B),
    if 
        Size == 0 -> {[], B1};
        true ->
            read_dict_content(B1, KT, VT, Size)
    end.

read_dict_content(_B, _, _, 0) ->
    null;

read_dict_content(B, KT, VT, Cnt) ->
    {KR, B1} = read(KT, B),
    {VR, B2} = read(VT, B1),
    case read_dict_content(B2, KT, VT, Cnt - 1) of
        null ->
            {[{KR, VR}], B2};
        {R, B3} ->
            {[{KR, VR}] ++ R, B3}
    end.

