%%
%% adapter.erl
%% Kevin Lynx
%% 05.06.2013
%%
-module(adapter).
-include("msg.hrl").
-compile(export_all).
-export([create/3, create/4]).
-export([invoke/5]).
-export([handle_data/3, handle_accept/2, handle_close/2]).
-behaviour(kl_tserver).
-record(adapter, {objects, id}).

%%
%% APIs
create(Id, Port, Objs) ->
    create(Id, undefined, Port, Objs).

% Objects format:
%   [{obj_name, [{method_name, Fun}, {method_name, Fun}]}, 
%       {obj_name, [{method_name, Fun}, {method_name, Fun}]}]
create(Id, IP, Port, Objs) ->
    object:validate_objects(Objs),
    A = #adapter{objects = Objs, id = Id},
    {ok, Pid} = kl_tserver:start_link(?MODULE, IP, Port, A),
    {ok, Pid}. 

%%
%% `kl_tserver' callbacks
handle_data(Sock, Data, State) ->
    conn:handle_data(State, Sock, Data),
    {ok, State}.

handle_accept(Sock, State) ->
    conn:handle_accept(Sock),
    {ok, State}.

handle_close(_Sock, _State) ->
    ok.

%%
%% `conn' callbacks
invoke(A, ObjName, MName, Arg, Sock) ->
    case object:find(A#adapter.objects, ObjName) of
        null -> ?REPLY_OBJ_NOT_FOUND;
        Obj -> 
            case object:find_method(Obj, MName) of
                null -> ?REPLY_METHOD_NOT_FOUND;
                Fun -> do_invoke(A, Fun, Arg, Sock)
            end
    end.

do_invoke(_A, Fun, Arg, _Sock) ->
    Ret = Fun(Arg),
    {?REPLY_SUCCESS, Ret}.

