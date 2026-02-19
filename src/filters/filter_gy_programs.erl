-module(filter_gy_programs).
-export([gy_programs/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

gy_programs(undefined, _Context) -> "";
gy_programs(#{<<"gr">> := #{<<"grades">> := Grades}}, _Context) ->
    Grades;
gy_programs(#{<<"gran">> := #{<<"grades">> := Grades}}, _Context) ->
    Grades;
gy_programs(#{<<"sam">> := #{<<"grades">> := Grades}}, _Context) ->
    Grades;
gy_programs(#{<<"gy">> := #{<<"programmes">> := Grades}}, _Context) ->
    Grades;
gy_programs(#{<<"gyan">> := #{<<"programmes">> := Grades}}, _Context) ->
    Grades;
gy_programs([ProgramMap|_T], _Context) ->
    F = fun({<<"Ak",_binary>> = Key,true}) -> {true, Key};
           (_) -> false
        end,
    lists:join(<<", ">>,lists:sort(lists:filtermap(F, maps:to_list(ProgramMap)))).
