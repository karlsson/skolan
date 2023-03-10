-module(filter_format_sefloat).
-export([format_sefloat/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

format_sefloat(Float, _Context) when is_float(Float)->
    String = io_lib:format("~.1f",[Float]),
    string:replace(String, ".", ",");
format_sefloat(Arg, _Context) ->
    Arg.

