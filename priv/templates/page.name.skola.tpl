{% extends "page.tpl" %}

{% block content_after %}
    <div class="page-relations">
        {# {% print m.req.qs %} #}
        {% with m.search::%{
           'facet.su_status':m.req.qs|make_filter:'facet.su_status',
           'facet.su_typ':m.req.qs|make_filter:'facet.su_typ',
           'facet.kommun':m.req.qs|make_filter:'facet.kommun',
            cat: id,
            sort: ["pivot_title"],
            pagelen: 20,
            page: q.page
            } as result
        %}
        <div class="connections paged" id="content-pager">
            <h3>
                {_ All _} <span>{{ id.title }}</span>
            </h3>
            <div class="list-items">
                {% for id in result %}
                    {% catinclude "_list_item.tpl" id %}
                {% endfor %}
            </div>
        {% pager result=result id=id qargs hide_single_page %}
        </div>
        {% endwith %}
    </div>
{% endblock %}
