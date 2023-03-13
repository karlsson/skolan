-module(filter_make_filter).
-export([make_filter/3, make_filter/4]).
-include_lib("zotonic_core/include/zotonic.hrl").

make_filter([{<<"q",Name/binary>>, Value}|T], Name, Context) ->
    [Value | make_filter(T, Name, Context)];
make_filter([{_, _}|T], Name, Context) ->
    make_filter(T, Name, Context);
make_filter([], _, _Context) ->
    [].

make_filter(QueryPart, Name, Pre, Context) ->
    case make_filter(QueryPart, Name, Context)of
        [] ->
            [];
        Result ->
            [Pre, Result]
    end.
