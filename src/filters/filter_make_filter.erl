-module(filter_make_filter).
-export([make_filter/3]).
-include_lib("zotonic_core/include/zotonic.hrl").

make_filter([{<<"q",Name/binary>>, Value}|T], Name, Context) ->
    [Value | make_filter(T, Name, Context)];
make_filter([{_, _}|T], Name, Context) ->
    make_filter(T, Name, Context);
make_filter([], _, _Context) ->
    [].

