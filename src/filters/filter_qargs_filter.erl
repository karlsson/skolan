-module(filter_qargs_filter).
-export([qargs_filter/2]).
-include_lib("zotonic_core/include/zotonic.hrl").


qargs_filter([[<<"q",_Name/binary>>, <<>>]|T], Context) ->
    qargs_filter(T, Context);
qargs_filter([[<<"q",Name/binary>> = A, Value]|T], Context) ->
    [<<A/binary, "=", Value/binary >> | qargs_filter(T, Context)];
qargs_filter([[_, _]|T], Context) ->
    qargs_filter(T, Context);
qargs_filter([], _Context) ->
    [];
qargs_filter(undefined, _Context) ->
    [].
