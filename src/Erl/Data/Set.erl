%%% Starting in OTP 24, the sets module has an option to represent sets as maps.
%%% This FFI uses that representation.
%%% See https://www.erlang.org/doc/system/maps.html#using-maps-as-sets
%%%
-module(erl_data_set@foreign).

-export([ delete/2
        , difference/2
        , empty/0
        , eqImpl/2
        , filter/2
        , findMax/1
        , findMin/1
        , foldImpl/3
        , fromMap/1
        , insert/2
        , intersection/2
        , isEmpty/1
        , map/2
        , mapMaybe/2
        , member/2
        , properSubset/2
        , singleton/1
        , size/1
        , subset/2
        , toggle/2
        , toList/1
        , toMap/1
        , union/2
        ]).

-define(just(X), {just, X}).
-define(nothing, {nothing}).
-define(unit, {unit}).

empty() ->
    sets:new([{version, 2}]).

isEmpty(S) ->
    sets:is_empty(S).

singleton(A) ->
    sets:add_element(A, empty()).

map(Fn, S) ->
    sets:fold(fun(E, Acc) -> sets:add_element(Fn(E), Acc) end, empty(), S).

member(A, S) ->
    sets:is_element(A, S).

insert(A, S) ->
    sets:add_element(A, S).

delete(A, S) ->
    sets:del_element(A, S).

toggle(A, S) ->
    case sets:is_element(A, S) of
        true ->
            sets:del_element(A, S);
        false ->
            sets:add_element(A, S)
    end.

size(S) ->
    sets:size(S).

findMin(S) ->
    sets:fold(fun(E, Acc) ->
                 case Acc of
                     ?nothing -> ?just(E);
                     ?just(Min) -> ?just(min(E, Min))
                 end
              end,
              ?nothing,
              S).

findMax(S) ->
    sets:fold(fun(E, Acc) ->
                 case Acc of
                     ?nothing -> ?just(E);
                     ?just(Max) -> ?just(max(E, Max))
                 end
              end,
              ?nothing,
              S).

union(S1, S2) ->
    sets:union(S1, S2).

difference(S1, S2) ->
    sets:subtract(S1, S2).

subset(S1, S2) ->
    sets:is_subset(S1, S2).

properSubset(S1, S2) ->
    ?MODULE:size(S1) < ?MODULE:size(S2) andalso sets:is_subset(S1, S2).

intersection(S1, S2) ->
    sets:intersection(S1, S2).

filter(Fn, S) ->
    sets:filter(Fn, S).

mapMaybe(Fn, S) ->
    sets:fold(fun(E, Acc) ->
                 case Fn(E) of
                     ?nothing -> Acc;
                     ?just(V) -> sets:add_element(V, Acc)
                 end
              end,
              empty(),
              S).
%% Probably safe with just "unsafe coerce" for both toMap and fromMap
%% but this is clearer.
toMap(S) ->
    maps:map(fun(_,_) -> ?unit end, S).

fromMap(S) ->
    maps:map(fun(_,_) -> [] end, S).


toList(S) ->
    sets:to_list(S).

foldImpl(Fn, Acc, S) ->
    sets:fold(Fn, Acc, S).

eqImpl(S1, S2) ->
    S1 =:= S2.
