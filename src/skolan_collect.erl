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
%%   If school unit does not exist
%%      add school unit
%%      add edge school_unit (has) huvudman privat|koncern|kommun.

-export([get_admin_context/0, add_all_huvudmen/0,
         get_all_huvudmen/1, get_all_jurper/1,
         get_all_su/1, update_all_su_from_remote/1, update_su/2,
         update_all_salsa/1, update_salsa/2,
         check_if_koncern/1, check_if_koncern/2,
         add_huvudmen/2,
         add_schools_for_all_huvudmen/1,
         add_schools_for_huvudmen/2, add_schools_for_huvudman/2,
         get_koncern/1, get_stored_item/3,
         stored_school_unit_nos/1, stored_school_unit_nos/2,
         get_all_su_kod/1, get_all_su_kod_from_remote/1,
         remove_old_zombies/1
        ]).

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
            %% Fix possible missing connection to koncern
            case {m_rsc:o(CompId, i_koncern, Context),
                  m_rsc:is_cat(CompId, koncern, Context)}
            of
                {[], false} ->
                    case check_if_koncern(OrgNo) of
                        {ok, CompId} -> % Koncern itself
                            do_nothing;
                        {ok, KoncernId} ->
                            {ok, _EdgeId} = m_edge:insert(CompId, i_koncern, KoncernId, Context);
                        _ ->
                            do_nothing
                    end;
                {_,_} -> %% Either koncern itself or already connected
                    do_nothing
            end,
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
            %% add_schools_for_huvudman(CompId, Context),
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
    add_schools_for_huvudman(CompId, Context),
    add_schools_for_huvudmen(T, Context).


%% Add school units for one huvudman
-spec add_schools_for_huvudman(CompId::integer(), z:context()) -> ok | {error, term()}.
add_schools_for_huvudman(CompId, Context) when is_integer(CompId)->
    Name = m_rsc:p(CompId, name, Context),
    OrgNo = orgno_from_name(Name),
    case z_utils:only_digits(OrgNo) of
        true ->
            case m_skolan_verket:fetch_data(skolenheter, OrgNo, Context) of
                [] -> do_nothing;
                L when is_list(L) ->
                    lists:foreach(
                      fun
                          (#{<<"Skolenhetskod">> := SchoolUnitCode,
                             <<"Skolenhetsnamn">> := Title,
                             <<"Status">> := Status}) ->
                              {ok, SchoolId} =
                                  create_if_not_exist(SchoolUnitCode, Title,
                                                      skola,[{status, Status}], Context),
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

%% ---------------------------------------------------------------
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

%% ---------------------------------------------------------------
%% School units
get_all_su(Context) ->
    get_all_su(1, [], Context).

get_all_su(Page, Acc, Context) ->
    case m_search:search(<<"query">>,
                         #{<<"cat">> => <<"skola">>,
                           <<"page">> => Page,
                           <<"pagelen">> => 1000}, Context) of
        {ok, #search_result{result = Result1, next = false }} ->
            Acc ++ Result1;
        {ok, #search_result{result = Result2, next = NextPage}} ->
            get_all_su(NextPage, Acc ++ Result2, Context)
    end.

get_all_su_kod(Context) ->
    [string:slice(m_rsc:p(A,<<"name">>, Context), 2) || A <- get_all_su(Context)].

get_all_su_kod_from_remote(Context) ->
    SUs = m_skolan_verket:fetch_data(skolenhet, Context),
    [SchoolUnitCode || #{<<"Skolenhetskod">> := SchoolUnitCode} <- SUs].
remove_old_zombies(Context) ->
    Zombies = get_all_su_kod(Context) -- get_all_su_kod_from_remote(Context),
    lists:foreach(fun(SchoolUnitCode) -> update_su(SchoolUnitCode, Context) end, Zombies).

update_all_su_from_remote(Context) ->
    SUs = m_skolan_verket:fetch_data(skolenhet, Context),
    lists:foreach(
      fun(#{<<"Skolenhetskod">> := SchoolUnitCode }) ->
              update_su(SchoolUnitCode, Context),
              timer:sleep(200)
      end, SUs).

update_su(Id, Context) when is_integer(Id) ->
    update_su(m_rsc:p(Id, name, Context), Context);
update_su(<<"se", SchoolUnitCode/binary>>, Context) ->
    update_su(SchoolUnitCode, Context);
update_su(SchoolUnitCode, Context) when is_binary(SchoolUnitCode)->
    try
        M = m_skolan_verket:fetch_data(skolenhet, SchoolUnitCode, Context),
        #{<<"Huvudman">> := #{<<"Typ">> := _Typ,
                              <<"PeOrgNr">> := _PeOrgNr,
                              <<"Namn">> := _Namn} = HuvudMan,
          <<"Status">> := Status,
          <<"Namn">> := Title,
          <<"Besoksadress">> :=
              #{<<"GeoData">> :=
                    #{<<"Koordinat_WGS84_Lat">> := Lat,
                     <<"Koordinat_WGS84_Lng">> := Lng
                     }
               }
         } = M,
        {ok, Id} =
            create_if_not_exist(SchoolUnitCode, Title,
                                skola,[{status, Status}], Context),
        RSC = m_rsc:get(Id, Context),

        {ok, HuvudManId} = add_huvudman(HuvudMan, Context),

        case m_rsc:o(Id, huvudman, Context) of
            [HuvudManId] ->
                ok;
            [] ->
                m_edge:insert(Id, huvudman, HuvudManId, Context); %% {ok, _EdgeId}
            HuvudMans -> %% There is a new huvudman
                Removes = HuvudMans -- [HuvudManId], %% Just to be sure not to remove any right one
                lists:foreach(fun(HId) -> m_edge:delete(Id, huvudman, HId, Context) end, Removes),
                m_edge:insert(Id, huvudman, HuvudManId, Context) %% {ok, _EdgeId}
        end,

        Statistics =
            case Status of
                <<"Aktiv">> ->
                    m_skolan_verket:fetch_data(statistics,
                                               SchoolUnitCode,
                                                Context);
                _ ->
                    undefined
            end,
        NewRSC = RSC#{<<"statistics">> => Statistics,
                      <<"skolenhet">> => M},
        NewRSC2 = case {Lat, Lng} of
                      {undefined, undefined} ->
                          NewRSC;
                      {_,_} ->
                          NewRSC#{
                                  <<"location_lat">> =>
                                      skolan_utils:bstring_to_float(Lat),
                                  <<"location_lng">> =>
                                      skolan_utils:bstring_to_float(Lng)}
                  end,
        m_rsc:update(Id, NewRSC2, Context)
    catch
        error:{badmatch, {error, {S, _, _, _, _}}}
          when S == 410; S == 400; S == 404 ->
            case m_rsc:name_to_id(
                          <<"se", SchoolUnitCode/binary>>,
                          Context) of
                {ok, Id2} ->
                    ?LOG_INFO(#{
                        text => <<"SchoolUnitCode not found at Skolverket - removing from database">>,
                        id => Id2,
                        school_unit_code => SchoolUnitCode,
                        reason => not_found
                    }),
                    m_rsc:delete(Id2, Context);
                {error,{unknown_rsc,_}} ->
                    do_nothing
            end;
        _:Error ->
            ?LOG_ERROR(#{
                        text => <<"Error updating school unit">>,
                        school_unit_code => SchoolUnitCode,
                        reason => Error
                       }),
            Error
    end.

update_all_salsa(Context) ->
    Salsas = m_skolan_verket:fetch_data(salsa, Context),
    lists:foreach(fun(Salsa) -> update_salsa(Salsa, Context) end, Salsas).

update_salsa(#{<<"schoolUnitCode">> := SUC} = Salsa, Context) ->
    m_rsc:update(<<"se", SUC/binary>>,
                 #{<<"salsa">> => Salsa}, Context).
%% ---------------------------------------------------------------

-spec create_if_not_exist(binary(), binary(), atom(), z:context())->
          {ok, integer()} | {error, term()}.
create_if_not_exist(OrgNo, Title, Category, Context)->
    create_if_not_exist(OrgNo, Title, Category, [], Context).


-spec create_if_not_exist(binary(), binary(), atom(), proplists:proplist(), z:context())->
          {ok, integer()} | {error, term()}.
create_if_not_exist(OrgNo, Title, Category, OptionalArgs, Context)->
    case get_stored_item(OrgNo, Category, Context) of
        {ok, Id} ->
            case proplists:get_value(status, OptionalArgs) of
                undefined ->
                    do_nothing;
                Status ->
                    m_rsc:update(Id, #{<<"status">> => Status}, Context)
            end,
            {ok, Id};
        {error,{unknown_rsc,_}} ->
            CatId = m_rsc:rid(Category, Context),
            Props1 =
                #{
                  <<"title">> => {trans,[{sv,Title}]},
                  <<"name">> => name_from_orgno(OrgNo, Category),
                  <<"category_id">> => CatId,
                  <<"tz">> => <<"UTC">>,
                  <<"is_published">> => true,
                  %% <<"creator_id">> => AdminId,
                  <<"language">> => [sv]
                 },
            Props =
                case proplists:get_value(status, OptionalArgs) of
                    undefined -> Props1;
                    Status -> Props1#{<<"status">> => Status}
                end,
            m_rsc:insert(Props, Context)
    end.

-spec get_stored_item( binary(), atom(), z:context() ) ->
          {ok, integer()}| {error,{unknown_rsc, binary()}}.
get_stored_item(OrgNo, Category, Context) ->
    m_rsc:name_to_id(name_from_orgno(OrgNo, Category), Context).

-spec orgno_from_name( binary() ) -> binary().
orgno_from_name(<< "se", OrgNo/binary >>) ->
    OrgNo;
orgno_from_name(<< "org", OrgNo/binary >>) -> %% jurper, privat, koncern, kommun
    OrgNo.

-spec name_from_orgno( binary(), atom() ) -> binary().
name_from_orgno(OrgNo, skola) ->
    <<"se", OrgNo/binary>>;
name_from_orgno(OrgNo, _) -> %% jurper, privat, koncern, kommun
    <<"org", OrgNo/binary>>.

stored_school_unit_nos(Context) ->
    stored_school_unit_nos(get_all_su(Context), Context).
stored_school_unit_nos(SchoolUnitIds, Context) ->
    Names = [m_rsc:p(Id, name, Context) || Id <- SchoolUnitIds],
    [NO || <<"se", NO/binary >> <- Names ].
