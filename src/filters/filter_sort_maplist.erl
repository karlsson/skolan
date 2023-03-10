-module(filter_sort_maplist).
-export([sort_maplist/3]).
-include_lib("zotonic_core/include/zotonic.hrl").

sort_maplist(MapList, Key, _Context) ->
    F = fun(A,B) ->
                #{Key := V1} = A,
                #{Key := V2} = B,
                V1 =< V2
        end,
    lists:sort(F,MapList).

