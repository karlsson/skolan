-module(filter_gy_programs).
-export([gy_programs/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

gy_programs([ProgramMap|_T], _Context) ->
    F = fun({Key,true}) -> {true, Key};
           (_) -> false
        end,
    lists:join(<<", ">>,lists:sort(lists:filtermap(F, maps:to_list(ProgramMap)))).
    
