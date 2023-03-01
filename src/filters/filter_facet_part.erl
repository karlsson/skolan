-module(filter_facet_part).
-export([facet_part/2]).
-include_lib("zotonic_core/include/zotonic.hrl").

facet_part(#search_result{facets = F}, _Context) -> F.
