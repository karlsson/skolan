-module(filter_is_checked).
-export([is_checked/4]).
-include_lib("zotonic_core/include/zotonic.hrl").

is_checked(Value, Name, [{Name, Value}|_], _Context) -> true;
is_checked(Value, Name, [{_, _}|T], Context) -> is_checked(Value, Name, T, Context);
is_checked(Value, Name, [[Name, Value]|_], _Context) -> true;
is_checked(Value, Name, [[_, _]|T], Context) -> is_checked(Value, Name, T, Context);
is_checked(_Value, _Name, [], _Context) -> false;
is_checked(_Value, _Name, undefined, _Context) -> false.
