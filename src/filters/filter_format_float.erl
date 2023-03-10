-module(filter_format_float).
-export([format_float/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

format_float(Float, _Context) when is_float(Float)->
    io_lib:format("~.2f",[Float]);
format_float(_, _Context) ->
    <<"99.99">>.

