-module(filter_make_filter).
-export([make_filter/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

make_filter([{<<"qfacet.",_/binary>> = Name1, Value}|T], Context) ->
    <<"q", Name/binary >> = Name1,
    [[Name, Value] | make_filter(T, Context)];
make_filter([{_, _}|T], Context) ->
    make_filter(T, Context);
make_filter([], _Context) ->
    [].

