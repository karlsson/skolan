{# {% all include facet.tpl %} #}
{# Faceted search - define blocks which will be mapped to facets    #}
{# The facets can be searched using the "facet" query.              #}

{# The block name is used to query the facets, define the type, and #}
{# report the facet back after quering.                             #}
{# A facet called "foo_int" will be called "foo" in searches and    #}
{# reports. Note "is_foo" will still be called "is_foo".            #}

{# The following types are available:                               #}
{# - ..._int          (integer number)                              #}
{# - ..._float        (floating point number)                       #}
{# - ..._date         (datetime in UTC)                             #}
{# - ..._ft           (fulltext search)                             #}
{# - ..._id           (resource id)                                 #}
{# - is_...           (boolean value)                               #}
{# - ..._range_float  (floating point number for minmax reporting)  #}
{# - ..._range_int    (integer number for minmax reporting)         #}
{# - ..._range_date   (date for minmax reporting)                   #}
{# - ..._list         (|| separated list of string values)          #}
{# - ..._ids          (|| separated list of resource ids)           #}

{# All others are mapped to text. Texts are always truncated at 80  #}
{# characters for their column index. Full text blocks are using    #}
{# two columns. One for the value, and one for a normalized value.  #}
{# A trigram index is added on the normalized column.               #}

{# Date(time)s are stored as-is, no additional timezone conversion  #}
{# is done, so be careful with the timezone used to fill the block. #}

{# If you need to return a label different than the value, then a   #}
{# block with prefix "label_" can be added. This block is rendered  #}
{# with the id of a resource that generated the given value.        #}
{# For blocks with postfix "_id" the default label is the resource  #}
{# title, or for persons the rendering of "_name.tpl"               #}

{% block su_status %}{{ id.status }}{% endblock %}
{% block su_typ %}{{ id.skolenhet.Huvudman.Typ }}{% endblock %}
{% block label_kommun %}{{ id.skolenhet.Kommun.Namn }}{% endblock %}
{% block kommun %}{{ id.skolenhet.Kommun.Kommunkod }}{% endblock %}
{% block gy_weighted_float %}{% for stats in id.statistics %}{% if stats.type == "gy" and stats.weightedStudentsPerTeacherQuota %}{{ stats.weightedStudentsPerTeacherQuota|format_float }}{% endif %}{% endfor %}{% endblock %}
{% block gr_weighted_float %}{% for stats in id.statistics %}{% if stats.type == "gr" and stats.weightedStudentsPerTeacherQuota %}{{ stats.weightedStudentsPerTeacherQuota|format_float }}{% endif %}{% endfor %}{% endblock %}
