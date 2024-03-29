-module(m_skolan_verket).
-behaviour(zotonic_model).

-export([
         m_get/3,
         fetch_data/2, fetch_data/3, fetch_data/4,
         fetch_school_unit_nos/1
        ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(API_URL, "https://api.skolverket.se/").
-define(SK_ENHET_REG, "skolenhetsregistret/v1").
-define(PE, "planned-educations/v3").

-spec m_get(Keys, Msg, Context) -> Return when
    Keys :: list(),
    Msg :: zotonic_model:opt_msg(),
    Return :: zotonic_model:return(),
    Context:: z:context().
%% Syntax: m.skolan_verket.api_url
m_get([ <<"api_url">> | Rest ], _Msg, _Context) ->
  {ok, {z_convert:to_binary(?API_URL), Rest}};

%% m.skolan_verket.huvudman
m_get([<<"huvudman">>], _Msg, Context) ->
  {ok, {fetch_data(huvudman,Context),[]}};
m_get([<<"huvudman">>, OrgNo], _Msg, Context) ->
  {ok, {fetch_data(huvudman, OrgNo, Context),[]}};
m_get([<<"huvudman">>,OrgNo, SearchDate], _Msg, Context) ->
  {ok, {fetch_data(huvudman, OrgNo, SearchDate, Context),[]}};

m_get([<<"skolenhet">>], _Msg, Context) ->
  {ok, {fetch_data(skolenhet, Context),[]}};
m_get([<<"skolenhet">>, SchoolUnitCode], _Msg, Context) ->
  {ok, {fetch_data(skolenhet, SchoolUnitCode, Context),[]}};
m_get([<<"skolenhet">>, <<"huvudman">>, OrgNo], _Msg, Context) ->
  {ok, {fetch_data(skolenheter, OrgNo, Context),[]}};
m_get([<<"skolenhet">>, SchoolUnitCode, SearchDate], _Msg, Context) ->
  {ok, {fetch_data(skolenhet, SchoolUnitCode, SearchDate, Context),[]}};
m_get([<<"diff">>, <<"huvudman">>, Date], _Msg, Context) ->
  {ok, {fetch_data(diff, huvudman, Date, Context),[]}};
m_get([<<"diff">>, <<"skolenhet">>, Date], _Msg, Context) ->
  {ok, {fetch_data(diff, skolenhet, Date, Context),[]}};


%% m.skolan_verket[jurperid.name]
m_get([<<"org", OrgNo/binary>>], _Msg, Context) ->
  {ok, {fetch_data(skolenheter, OrgNo, Context),[]}};
%% m.skolan_verket[skolenhetid.name]
m_get([<<"se", SchoolUnitCode/binary>>], _Msg, Context)->
  {ok, {fetch_data(statistics, SchoolUnitCode, Context),[]}};
m_get([<<"national_values">>, SchoolForm], _Msg, Context)->
  {ok, {fetch_data(national_values, SchoolForm, Context),[]}};
m_get([<<"salsa">>], _Msg, Context) ->
  {ok, {fetch_data(salsa, Context),[]}};
m_get([<<"program">>], _Msg, Context) ->
  {ok, {fetch_data(program, Context),[]}};
m_get(_, _Msg, _Context) ->
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

-spec fetch_data(Type, Context) -> Return when
    Type :: huvudman | skolenhet,
    Return :: zotonic_model:return(),
    Context:: z:context().
fetch_data(Type, Context)->
  z_depcache:memo(
    fun() ->
        fetch_data1(Type, Context)
    end,
    {Type},
    Context).
%% -----------------
fetch_school_unit_nos(Context) ->
  [SUNO || #{<<"Skolenhetskod">> := SUNO} <- fetch_data(skolenhet, Context)].

-spec fetch_data(Type, Arg, Context) -> Return when
    Type :: huvudman | skolenhet | skolenheter | national_values | statistics,
    Arg :: SchoolUnitCode | OrgNo | SchoolForm,
    SchoolUnitCode :: binary(),
    OrgNo :: binary(),
    SchoolForm :: binary(),
    Return :: zotonic_model:return(),
    Context:: z:context().
fetch_data(Type, Arg, Context) ->
  z_depcache:memo(
    fun() ->
        fetch_data1(Type, Arg, Context)
    end,
    {Type, Arg},
    10*?MINUTE,
    Context).

-spec fetch_data(Type, Arg, SearchDate, Context) -> Return when
    Type :: huvudman | skolenhet | diff,
    Arg :: SchoolUnitCode | OrgNo,
    SchoolUnitCode :: binary(),
    OrgNo :: binary(),
    SearchDate :: binary(), %% <<"20230301">>
    Return :: zotonic_model:return(),
    Context:: z:context().

fetch_data(Type, Arg, SearchDate, Context) ->
  z_depcache:memo(
    fun() ->
        fetch_data1(Type, Arg, SearchDate, Context)
    end,
    {Type, Arg},
    10*?MINUTE,
    Context).

fetch_options() -> [{timeout, 10000}].

%% Get all huvudmen
fetch_data1(huvudman, Context) ->
    {ok, A} = z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/huvudman",
                                 fetch_options(), Context),
    %% Strip of first since it is a dummy
    tl(maps:get(<<"Huvudman">>, A));

%% Get all school units
fetch_data1(skolenhet, Context) ->
    {ok, A} = z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/skolenhet",
                               fetch_options(), Context),
    maps:get(<<"Skolenheter">>, A);

%% Get salsa values for all school units that have them.
%%   "salsaAverageGradesIn9thGradeActual"
%%   "salsaAverageCalculated"
%%   "salsaAverageGradesIn9thGradeDeviation"
%%   "salsaRequirementsReachedActual"
%%   "salsaRequirementsReachedCalculated"
%%   "salsaRequirementsReachedDeviation"
%%   "salsaNewlyImmigratedQuota"
%%   "salsaBoysQuota"
%%   "salsaParentsEducation"
fetch_data1(salsa, Context) ->
    {ok, #{<<"body">> := A}}
        = fetch_hal_json(?API_URL ++ ?PE ++ "/statistics/all-schools/salsa",
                         fetch_options(), Context),
    maps:get(<<"compulsorySchoolUnitSalsaMetricList">>, A);

fetch_data1(program, Context) ->
     case fetch_hal_json(?API_URL ++ ?PE ++ "/support/programs",
                       fetch_options(), Context) of
         {ok, #{<<"body">> := #{<<"gy">> := A}}} -> A;
         _ -> []
     end.


fetch_data1(huvudman, OrgNo, Context) ->
    {ok, A} =
        z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/huvudman/" ++
                               binary_to_list(OrgNo),
                           fetch_options(), Context),
    maps:get(<<"Huvudman">>, A);

fetch_data1(skolenhet, SchoolUnitCode, Context) ->
    {ok, A} =
        z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/skolenhet/" ++
                               binary_to_list(SchoolUnitCode),
                           fetch_options(), Context),
    maps:get(<<"SkolenhetInfo">>, A);

%% Get all school units for one huvudman
fetch_data1(skolenheter, OrgNo, Context) ->
    {ok, A} =
        z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/skolenhet/huvudman/" ++
                               binary_to_list(OrgNo),
                           fetch_options(), Context),
    maps:get(<<"Skolenheter">>, A);

fetch_data1(national_values, <<"gy">>, Context) ->
  fetch_data1(national_values, <<"gy/NA">>, Context);
fetch_data1(national_values, SchoolForm, Context) ->
  Options = [{'x-api-version', "v3"} | fetch_options()],
  case
    fetch_hal_json(?API_URL ++ ?PE ++ "/statistics/national-values/" ++
                       binary_to_list(SchoolForm), Options, Context)
  of
    {ok, #{<<"body">> := B}} ->
      STP = case maps:find(<<"studentsPerTeacherQuota">>, B) of
              {ok, [#{<<"value">> := Q1}|_] }-> Q1;
              _ -> <<"-">>
            end,
      CTQ = case maps:find(<<"certifiedTeachersQuota">>, B) of
              {ok, [#{<<"value">> := Q2}|_]} -> Q2;
              _ -> <<"-">>
            end,
      #{<<"studentsPerTeacherQuota">> => STP,
        <<"certifiedTeachersQuota">> => CTQ,
       <<"weightedStudentsPerTeacherQuota">> =>
            skolan_utils:weighted_pup_per_teacher(STP, CTQ)};
    _Error ->
      <<"">>
  end;

fetch_data1(statistics, SchoolUnitCode, Context) ->
    {ok, #{<<"body">> := #{<<"_links">> := LinkMap}}} =
        fetch_hal_json(?API_URL ++ ?PE ++ "/school-units/" ++
                           binary_to_list(SchoolUnitCode)  ++ "/statistics" ,
                       fetch_options(), Context),
    Links = maps:to_list(LinkMap),
    F = fun
            ({<<"self">>, _}) ->
                false;
            ({TS, Link}) -> % TS is <<"{gr,grs,gy,gys,fs,fsk,..}-statistics">>
                case string:split(TS,"-") of
                    [Type,_] ->
                        {true, get_statistics(Type, SchoolUnitCode, Link, Context)};
                    _ -> false
                end
        end,
    lists:filtermap(F, Links).

fetch_data1(huvudman, OrgNo, SearchDate, Context) ->
    {ok, A} =
        z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/huvudman/" ++
                               binary_to_list(OrgNo) ++ "/" ++ SearchDate,
                           fetch_options(), Context),
    maps:get(<<"Huvudman">>, A);

fetch_data1(skolenhet, SchoolUnitCode, SearchDate, Context) ->
    {ok, A} =
        z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/skolenhet/" ++
                               binary_to_list(SchoolUnitCode) ++ "/" ++ SearchDate,
                           fetch_options(), Context),
    maps:get(<<"SkolenhetInfo">>, A);

fetch_data1(diff, huvudman, Date, Context) ->
    {ok, A} =
        z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/diff/huvudman/" ++
                               binary_to_list(Date),
                           fetch_options(), Context),
    maps:get(<<"PeOrgNr">>, A);
fetch_data1(diff, skolenhet, Date, Context) ->
    {ok, A} =
        z_fetch:fetch_json(?API_URL ++ ?SK_ENHET_REG ++ "/diff/skolenhet/" ++
                               binary_to_list(Date),
                           fetch_options(), Context),
    maps:get(<<"Skolenhetskoder">>, A).

get_statistics(Type, SchoolUnitCode, #{<<"href">> := Url},Context) ->
    {ok, #{<<"body">> := A}} =  fetch_hal_json(Url , fetch_options(), Context),
    StatMap = get_statistics1(Type, A),
    get_salsa_statistics(Type, StatMap, SchoolUnitCode, Context).


get_salsa_statistics(<<"gr">>, StatMap, SchoolUnitCode, Context) ->
  Url = ?API_URL ++ ?PE ++ "/statistics/all-schools/salsa/" ++ binary_to_list(SchoolUnitCode),
  case fetch_hal_json(Url , fetch_options(), Context) of
    {ok, #{<<"body">> := #{<<"compulsorySchoolUnitSalsaMetricList">> := [#{} = A|_]}}} ->
      case maps:find(<<"salsaAverageGradesIn9thGradeDeviation">>, A) of
          {ok, #{<<"value">> := Q1, <<"valueType">> := <<"EXISTS">>}} ->
            StatMap#{<<"salsaAverageDev">> => Q1};
          _ -> StatMap
        end;
    _Error ->
      StatMap
  end;
get_salsa_statistics(_, StatMap, _, _) -> StatMap.


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
  VForm = get_vform(Type),
  #{<<"type">> => Type,
    <<"vform">> => VForm,
    <<"studentsPerTeacherQuota">> => STP,
    <<"certifiedTeachersQuota">> => CTQ,
    <<"weightedStudentsPerTeacherQuota">> =>
        skolan_utils:weighted_pup_per_teacher(STP, CTQ),
    <<"hasLibrary">> => HasLibrary}.

%% 10 Förskola
%% 16 Pedagogisk omsorg
%% 15 Fritidshem
%% 17 Öppen förskola
%% 20 Öppen fritidsverksamhet
%% 14 Förskoleklass
%% 11 Grundskolan
%% 21 Gymnasieskolan
%% 12 Specialskola
%% 13 Grundsärskolan
%% 23 Gymnsasiesärskolan
%% 32 Komvux
%% 38 Kompletterande utbildningar
%% 86 Omsorg på obekväma tider
get_vform(<<"gr">>) -> 11;
get_vform(<<"gy">>) -> 21;
get_vform(<<"grs">>) -> 13;
get_vform(<<"gys">>) -> 23;
get_vform(<<"fsk">>) -> 14;
get_vform(<<"fs">>) -> 10;
get_vform(<<"fth">>) -> 15;
get_vform(<<"sp">>) -> 12;
get_vform(<<"vux">>) -> 32;
get_vform(_) -> 38.

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
