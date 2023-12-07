-module(filter_make_filter).
-export([make_filter/2, make_filter/3, make_filter/4]).
-include_lib("zotonic_core/include/zotonic.hrl").

make_filter(Payload, Context) ->
    case make_filter1(Payload, Context) of
        [] ->
            Payload;
        QPayload ->
            QPayload
    end.

make_filter([[<<"q",Name/binary>>, <<>>]|T], Name, Context) ->
    make_filter(T, Name, Context);
make_filter([{<<"q",Name/binary>>, Value}|T], Name, Context) ->
    [Value | make_filter(T, Name, Context)];
make_filter([[<<"q",Name/binary>>, Value]|T], Name, Context) ->
    [Value | make_filter(T, Name, Context)];
make_filter([[Name, Value]|T], <<"page">> = Name, Context) ->
    [Value | make_filter(T, Name, Context)];
make_filter([{_, _}|T], Name, Context) ->
    make_filter(T, Name, Context);
make_filter([[_, _]|T], Name, Context) ->
    make_filter(T, Name, Context);
make_filter([], _, _Context) ->
    [];
make_filter(undefined, _, _Context) ->
    [].

make_filter(QueryPart, Name, Pre, Context) ->
    case make_filter(QueryPart, Name, Context)of
        [] ->
            [];
        Result ->
            [Pre, Result]
    end.


make_filter1([[<<"qpayload">>, Pload]|_], _Context) ->
    split_pload(Pload);
make_filter1([_|T], _Context) ->
    make_filter1(T, _Context);
make_filter1([], _Context) ->
    [].

%% <<"qpayload=qfacet.su_status=Aktiv,qgyr_weighted=none,q_weighted=25">>
split_pload(Pload) when is_binary(Pload) ->
    split_pload(binary:split(Pload, <<",">>, [global]));
split_pload([Pload|T]) when is_binary(Pload) ->
    [binary:split(Pload, <<"=">>)|split_pload(T)];
split_pload([]) ->
    [].
