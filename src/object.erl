%%
%% object.erl
%% Kevin Lynx
%% 05.06.2013
%%
-module(object).
-compile(export_all).
-export([validate_objects/1, find/2, find_method/2]).

% objects list format:
%   [{name_string, methods}, {name_string, methods}]
validate_objects([Object | Rest]) ->
    validate_object(Object),
    validate_objects(Rest);

validate_objects([]) ->
    ok.

validate_object({Name, Methods}) when is_list(Name) ->
    validate_methods(Methods).

% methods list format:
%   [{method_name, fun}, {method_name, fun}]
validate_methods([Method | Rest]) ->
    validate_method(Method),
    validate_methods(Rest);

validate_methods([]) ->
    ok.

validate_method({Name, Fun}) when is_list(Name) and is_function(Fun) ->
    ok.

find([Object | Rest], Name) ->
    {ObjName, Methods} = Object,
    case Name =:= ObjName of
        true -> Methods;
        _ -> find(Rest, Name)
    end;

find([], _Name) ->
    null.

find_method(null, _Name) ->
    null;

find_method([], _Name) ->
    null;

find_method([Method | Rest], Name) ->
    {MName, Fun} = Method,
    case MName =:= Name of
        true -> Fun;
        _ -> find_method(Rest, Name)
    end.

