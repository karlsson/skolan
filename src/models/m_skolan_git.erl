-module(m_skolan_git).

-behaviour(zotonic_model).

-export([m_get/3]).

%% -include_lib("zotonic_core/include/zotonic.hrl").

m_get([<<"revision">>], _Msg, Context) ->
  {ok, m_get([<<"revision">>], Context)}.

m_get([<<"revision">>], Context) ->
  A = z_depcache:memo(
        fun() ->
            os:cmd("git rev-parse --short HEAD") -- "\n"
        end,
        {git_sha},
        Context),
  {A,[]}.
