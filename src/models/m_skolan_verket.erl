-module(m_skolan_verket).
-behaviour(zotonic_model).

-export([
         m_get/3,
         fetch_data/2, fetch_data/3
        ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(API_URL, "https://api.skolverket.se/").
-define(SK_ENHET_REG, "skolenhetsregistret/v1").
-define(PE, "planned-educations/v3").
-define(TIMEOUT, 10000).

-spec m_get(Keys, Msg, Context) -> Return when
    Keys :: list(),
    Msg :: zotonic_model:opt_msg(),
    Return :: zotonic_model:return(),
    Context:: z:context().
                                                % Syntax: m.skolan_verket.api_url
m_get([ <<"api_url">> | Rest ], _Msg, _Context) ->
  {ok, {z_convert:to_binary(?API_URL), Rest}};

%% m.skolan_verket.huvudman
m_get([<<"huvudman">>], _Msg, Context) ->
  {ok, {fetch_data(huvudman,Context),[]}};

%% m.skolan_verket[jurperid.name]
m_get([<<"org", OrgNo/binary>>], _Msg, Context) ->
  {ok, {fetch_data(skolenheter, OrgNo, Context),[]}};
%% m.skolan_verket[skolenhetid.name]
m_get([<<"se", SchoolUnitCode/binary>>], _Msg, Context)->
  {ok, {fetch_data(statistics, SchoolUnitCode, Context),[]}};
m_get(_, _Msg, Context) ->
  {error, <<"Argument skall vara, huvudman, org<organisationsnummer>, se<skolenhetskod. ",
            "Exempelvis org5568089246">>}.
%% m_get([Id], _Msg, Context)->
%%   case m_rsc:p(Id, name, Context) of
%%     undefined ->
%%       {error, not_found};
%%     <<"org", OrgNo/binary>> ->
%%       {ok, {fetch_data(skolenheter, OrgNo, Context),[]}};
%%     <<"se", SchoolUnitCode/binary>> ->
%%       {ok, {fetch_data(statistics, SchoolUnitCode, Context),[]}}
%%   end.

%% huvudman has Namn, PeOrgNr and Typ as attributes.
%% [#{Namn, PeOrgNr, Typ}] Typ = "Enskild"|"Kommun"|"Region"
fetch_data(huvudman, Context)->
  z_depcache:memo(
    fun() ->
        fetch_data1(huvudman, Context)
    end,
    {huvudman},
    Context).
fetch_data(skolenheter, OrgNo, Context) ->
  z_depcache:memo(
    fun() ->
        fetch_data1(skolenheter, OrgNo, Context)
    end,
    {skolenheter, OrgNo},
    10*?MINUTE,
    Context);

fetch_data(statistics, SchoolUnitCode, Context) ->
  fetch_data1(statistics, SchoolUnitCode, Context).

fetch_data1(huvudman, Context) ->
  Options = [{timeout, ?TIMEOUT}],
  case
    z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/huvudman", Options, Context)
  of
    {ok, A} ->
      %% Strip of first since it is a dummy
      tl(maps:get(<<"Huvudman">>, A));
    Error ->
      Error
  end.
fetch_data1(skolenheter, OrgNo, Context) ->
  Options = [{timeout, ?TIMEOUT}],
  case
    z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/skolenhet/huvudman/" ++ binary_to_list(OrgNo), Options, Context)
  of
    {ok, A} ->
      maps:get(<<"Skolenheter">>, A);
    Error ->
      Error
  end;
fetch_data1(statistics, SchoolUnitCode, Context) ->
  Options = [{timeout, ?TIMEOUT}],
  case
    fetch_hal_json(?API_URL ++ ?PE ++ "/school-units/" ++ binary_to_list(SchoolUnitCode)  ++ "/statistics" , Options, Context)
  of
    {ok, #{<<"body">> := #{<<"_links">> := LinkMap}}} ->
      Links = maps:to_list(LinkMap),
      F = fun
            ({<<"self">>, _}) ->
              false;
            ({TS, Link}) -> % TS is <<"{gr,grs,gy,gys,fs,fsk,..}-statistics">>
              case string:split(TS,"-") of
                [Type,_] ->
                  {true, get_statistics(Type, Link, Context)};
                _ -> false
              end
          end,
      lists:filtermap(F, Links);
    Error ->
      Error
  end.

get_statistics(Type, #{<<"href">> := Url},Context) ->
  Options = [{timeout, ?TIMEOUT}],
  case fetch_hal_json(Url , Options, Context) of
    {ok, #{<<"body">> := A}} ->
      get_statistics1(Type, A);
    Error ->
      Error
  end.

get_statistics1(Type, A) ->
  STP = case find_in_statistics(<<"studentsPerTeacherQuota">>, A) of
          {ok, [#{<<"value">> := Q1, <<"timePeriod">> := T1, <<"valueType">> := <<"EXISTS">>}|_]} ->
            <<Q1/binary, " - ", T1/binary >>;
          _Error1 -> <<"-">>
        end,
  CTQ = case find_in_statistics(<<"certifiedTeachersQuota">>, A) of
          {ok, [#{<<"value">> := Q2, <<"timePeriod">> := T2, <<"valueType">> := <<"EXISTS">>}|_]} ->
            <<Q2/binary, " - ", T2/binary >>;
          _Error2 -> <<"-">>
        end,
  HasLibrary = case find_in_statistics(<<"hasLibrary">>, A) of
                 {ok, Value3} -> case Value3 of true -> <<"Ja">>; false -> <<"Nej">> end;
                 _Error3 -> <<"-">>
               end,
  #{<<"type">> => Type,
    <<"studentsPerTeacherQuota">> => STP,
    <<"certifiedTeachersQuota">> => CTQ,
    <<"hasLibrary">> => HasLibrary}.

find_in_statistics(Key, Map) when is_map(Map)->
  case maps:find(Key, Map) of
    {ok, Value} -> {ok, Value};
    _ ->
      case maps:find(<<"programMetrics">>, Map) of
        {ok, PM} -> 
          find_in_statistics1(Key, PM);
        _ ->
          {error, not_found}
      end
  end.

find_in_statistics1(_Key, []) ->
  {error, not_found};
find_in_statistics1(Key, [H|T]) ->
  case maps:find(Key, H) of
    {ok, Value} -> {ok, Value};
    _ -> find_in_statistics1(Key, T)
  end.

fetch_hal_json(Url, Options, _Context) ->
  Url1 = z_convert:to_binary(Url),
  Options1 = [ {accept, "application/vnd.skolverket.plannededucations.api.v3.hal+json"} | Options ],
  case z_url_fetch:fetch(Url1, Options1) of
    {ok, {_Final, _Hs, _Length, <<>>}} ->
      {ok, #{}};
    {ok, {_Final, _Hs, _Length, Body}} ->
      {ok, jsxrecord:decode(Body)};
    {error, _} = Error ->
      Error
  end.
