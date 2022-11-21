-module(skolan_collect).

%% Scan skolverkets database on huvudman and attached school units.
%% If huvudman does not exist
%%   add huvudman (foretag)
%%   If huvudman is part of a Koncern (koncernmoderbolag)
%%     If koncern name does not exist add koncern
%%       add edge that huvudman is part of (ingar_i) koncern
%%
%%   Get all school units belonging to huvudman
%%   ( curl 'https://api.skolverket.se/skolenhetsregistret/v1/skolenhet/huvudman/{organisation_no}'
%%        -H 'accept: application/json')
%%   If school unit is active ("Status" == "Aktiv") and does not exist
%%      add school unit
%%      add edge huvudman Owns (ager) school unit.

-export([get_admin_context/0, add_all_huvudmen/0,
         get_all_huvudmen/1, get_all_jurper/1,
         check_if_koncern/1, check_if_koncern/2,
         add_huvudmen/2,
         add_schools_for_all_huvudmen/1,
         add_schools_for_huvudmen/2, add_schools/2,
         get_koncern/1, get_stored_item/3]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(SITE, skolan).

get_admin_context() ->
  Context = z_context:new(?SITE),
  {ok, AdminId} = m_rsc:name_to_id_cat(administrator, person, Context),
  z_acl:logon(AdminId, Context).

add_all_huvudmen() ->
  Context = get_admin_context(),
  AllMenFromSkolverket = get_all_huvudmen(Context),
  add_huvudmen(AllMenFromSkolverket, Context).
get_all_huvudmen(Context) ->
  m_skolan_verket:fetch_data(huvudman, Context).

get_all_jurper(Context) ->
  get_all_jurper(1, [], Context).
get_all_jurper(Page, Acc, Context) ->
  case m_search:search(<<"query">>,
                       #{<<"cat">> => <<"jurper">>,
                         <<"page">> => Page,
                         <<"pagelen">> => 300}, Context) of
    {ok, #search_result{result = Result1, next = false }} ->
      Acc ++ Result1;
    {ok, #search_result{result = Result2, next = NextPage}} ->
      get_all_jurper(NextPage, Acc ++ Result2, Context)
  end.

add_huvudmen(K, Context) ->
  add_huvudmen1(K, Context).

add_huvudmen1([], _Context) -> ok;
add_huvudmen1([H|T], Context) ->
  add_huvudman(H, Context),
  add_huvudmen1(T, Context).

add_huvudman(#{
               <<"Typ">> := <<"Enskild">>,
               <<"Namn">> := Name,
               <<"PeOrgNr">> := OrgNo}, Context) ->
  case get_stored_item(OrgNo, jurper, Context) of
    {ok, CompId} ->
      {ok, CompId};
    {error,{unknown_rsc,_}} ->
      timer:sleep(500),
      {Category, KoncernId} =
        case check_if_koncern(OrgNo, Context) of
          {ok, KId} -> {privat, KId};
          _ -> {privat, undefined}
        end,
      {ok, CompId} = create_if_not_exist(OrgNo, Name, Category, Context),
      case KoncernId of
        undefined -> do_nothing;
        CompId -> do_nothing; % The company itself is koncernmoderbolag
        _ ->
          {ok, _EdgeId} = m_edge:insert(CompId, i_koncern, KoncernId, Context)
      end,
      add_schools(CompId, Context),
      {ok, CompId}
  end;
add_huvudman(#{
               <<"Namn">> := Name,
               <<"PeOrgNr">> := OrgNo}, Context) ->
  timer:sleep(1000),
  create_if_not_exist(OrgNo, Name, kommun, Context).


%% Internal functions
check_if_koncern(OrgNo) ->
  Context = z_context:new(?SITE),
  check_if_koncern(OrgNo, Context).

check_if_koncern(OrgNo, Context) ->
  case get_koncern(OrgNo) of
    {ok, {KOrgNo, KName}} ->
      %% Create new koncern mother company.
      create_if_not_exist(KOrgNo, KName, koncern, Context);
    Error -> Error
  end.
-spec create_if_not_exist(binary(), binary(), atom(), z:context())->
        {ok, integer()} | {error, term()}.
create_if_not_exist(OrgNo, Title, Category, Context)->
  case get_stored_item(OrgNo, Category, Context) of
    {ok, Id} -> {ok, Id};
    {error,{unknown_rsc,_}} ->
      CatId = m_rsc:rid(Category, Context),
      Props = #{
                <<"title">> => {trans,[{sv,Title}]},
                <<"name">> => name_from_orgno(OrgNo, Category),
                <<"category_id">> => CatId,
                <<"tz">> => <<"UTC">>,
                <<"is_published">> => true,
                %% <<"creator_id">> => AdminId,
                <<"language">> => [sv]
               },
      m_rsc:insert(Props, Context)
  end.

%% Fetch koncern mother company (if any) for company id from "allabolag.se"
%% This involves scraping html... Can change
%% https://www.allabolag.se/{companyid}/koncern
get_koncern(OrgNo) when is_binary(OrgNo) ->
  get_koncern(binary_to_list(OrgNo));
get_koncern(OrgNo) when is_list(OrgNo)->
  Options = [{timeout, 10000}],
  Url = "https://www.allabolag.se/" ++ OrgNo ++ "/koncern",
  case z_url_fetch:fetch(Url, Options) of
    {ok, {_Final, _Hs, _Length, <<>>}} ->
      {error, empty};
    {ok, {_Final, _Hs, _Length, Html}} ->
      case string:find(Html, "Koncernmoderbolaget") of
        nomatch -> {error, nomatch};
        A ->
          case string:split(string:slice(A, 0, 150), "allabolag.se/") of
            [_,OrgNr1] ->
              [OrgNr2|_] = string:split(OrgNr1,"/"),
              [_,Konc1|_] = string:split(A,">"),
              [Konc2|_] = string:split(Konc1,"<"),
              {ok, {OrgNr2, Konc2}};
            [B1] ->
              [_, B2] = string:split(B1, "\n"),
              [B3|_] = string:split(B2, "\n"),
              B4 = string:trim(B3),
              B5 = binary:replace(string:lowercase(B4), <<" ">>,<<"_">>),
              {ok,{B5,B4}}
          end
      end;
    Error -> Error
  end.


add_schools_for_all_huvudmen(Context) ->
  AllPossibleCompIds = get_all_jurper(Context),
  add_schools_for_huvudmen(AllPossibleCompIds, Context).

add_schools_for_huvudmen([], _Context) -> ok;
add_schools_for_huvudmen([CompId|T], Context) ->
  add_schools(CompId, Context),
  add_schools_for_huvudmen(T, Context).


%% Add school units for one huvudman
-spec add_schools(CompId::integer(), z:context()) -> ok | {error, term()}.
add_schools(CompId, Context) when is_integer(CompId)->
  Name = m_rsc:p(CompId, name, Context),
  OrgNo = orgno_from_name(Name, jurper),
  case z_utils:only_digits(OrgNo) of
    true ->
      case m_skolan_verket:fetch_data(skolenheter, OrgNo, Context) of
        [] -> do_nothing;
        L when is_list(L) ->
          lists:foreach(
            fun
              (#{<<"Skolenhetskod">> := SchoolUnitCode,
                 <<"Skolenhetsnamn">> := Title,
                 <<"Status">> := <<"Aktiv">>}) ->
                {ok, SchoolId} = create_if_not_exist(SchoolUnitCode, Title, skola, Context),
                m_edge:insert(SchoolId, huvudman, CompId, Context); %% {ok, _EdgeId}
              (_) -> do_nothing
            end, L),
          timer:sleep(200);
        _ -> do_nothing
      end;
    false ->
      do_nothing
  end,
  ok.

-spec get_stored_item( binary(), atom(), z:context() ) ->
        {ok, integer()}| {error,{unknown_rsc, binary()}}.
get_stored_item(OrgNo, Category, Context) ->
  m_rsc:name_to_id(name_from_orgno(OrgNo, Category), Context).

-spec orgno_from_name( binary(), atom() ) -> binary().
orgno_from_name(Name, skola) ->
  << "se", OrgNo/binary >> = Name,
  OrgNo;
orgno_from_name(Name, _) -> %% jurper, privat, koncern, kommun
  << "org", OrgNo/binary >> = Name,
  OrgNo.

-spec name_from_orgno( binary(), atom() ) -> binary().
name_from_orgno(OrgNo, skola) ->
  <<"se", OrgNo/binary>>;
name_from_orgno(OrgNo, _) -> %% jurper, privat, koncern, kommun
  <<"org", OrgNo/binary>>.