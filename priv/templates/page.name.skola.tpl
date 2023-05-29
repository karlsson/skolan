{% extends "page.tpl" %}

{% block content_after %}
        {% with m.search.facets::%{
           'facet.su_status':m.req.qs|make_filter:'facet.su_status',
           'facet.su_typ':m.req.qs|make_filter:'facet.su_typ',
           'facet.kommun':m.req.qs|make_filter:'facet.kommun',
           'facet.gy_weighted':m.req.qs|make_filter:'gy_weighted':'<'|to_binary,
           'facet.gr_weighted':m.req.qs|make_filter:'gr_weighted':'<'|to_binary,
           'facet.is_salsa':m.req.qs|make_filter:'facet.is_salsa',
            text:q.qsu_title,
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
            <table class="table table-sm">
            <thead>
            <th scope="col">Skolenhet</th>
            <th scope="col">Status</th>
            <th scope="col">Typ</th>
            <th scope="col">Viktad<br/>elevtäthet</th>
            <th scope="col">Salsa</br>
              Föräldrarnas genomsnittliga utbildningsnivå : </br>
              Andel nyinvandrade (%) : </br>
              Andel pojkar(%) : </br>
              Residual för genomsnittligt meritvärde
            </th>
            </thead>
            <tbody>
            {% for id in result %}
                {% if id.is_visible %}
                <tr {% if id.status == "Aktiv" %} class="table-primary" {% endif %}>
                <td><a href="{{ id.page_url }}">{{ id.title|default:_"Untitled" }}</a></td>
        <td>{{ id.status|first:2 }}</td>
        <td>{{ id.skolenhet.Huvudman.Typ|first:2 }}</td>
        <td>
        {% for id1 in id.statistics %}
        {{ id1.type }}:{{ id1.weightedStudentsPerTeacherQuota|format_sefloat }}<br/>
        {% endfor %}
        </td>
        <td>
        {% if id.salsa %}
        {{ id.salsa.salsaParentsEducation.value }} :
        {{ id.salsa.salsaNewlyImmigratedQuota.value }} :
        {{ id.salsa.salsaBoysQuota.value }} :
        {{ id.salsa.salsaAverageGradesIn9thGradeDeviation.value }}
        {% endif %}
        </td>
    </tr>
    {% endif %}
            {% endfor %}
            </tbody>
            <table>
        {% pager result=result id=id qargs hide_single_page %}
        </div>
        {% endwith %}
{% endblock %}
