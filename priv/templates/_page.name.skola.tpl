{% with (q.payload|make_filter) as qpayload %}
    {% with m.search.facets::%{
           'facet.su_status':qpayload|make_filter:'facet.su_status',
           'facet.su_typ':qpayload|make_filter:'facet.su_typ',
           'facet.kommun':qpayload|make_filter:'facet.kommun',
           'facet.gy_weighted':(qpayload|make_filter:'gyr_weighted'|first == "gy")|
             if:(qpayload|make_filter:'gyrr_weighted':'<'|to_binary):"",
           'facet.gr_weighted':(qpayload|make_filter:'gyr_weighted'|first == "gr")|
             if:(qpayload|make_filter:'gyrr_weighted':'<'|to_binary):"",
           'facet.is_salsa':qpayload|make_filter:'facet.is_salsa',
            text:qpayload|make_filter:'su_title',
            cat: id,
            sort: ["pivot_title"],
            pagelen: 20,
            page: q.payload|make_filter:'page'|first
            } as result
        %}
        <div class="connections paged" id="content-pager">
            <h3>
                {_ All _} <span>{{ id.title }}</span>
            </h3>
            <table class="table table-sm">
            <thead>
            <th scope="col">Skolenhet ( Koncern )</th>
            <th scope="col">Status</th>
            <th scope="col">Typ</th>
            <th scope="col">Viktad<br/>elevtäthet</th>
            <th scope="col">Salsa (gr), Program (gy)
              <div>Föräldrarnas genomsnittliga utbildningsnivå : </div>
              <div>Andel nyinvandrade (%) : </div>
              <div>Andel pojkar(%) : </div>
              <div>Residual för genomsnittligt meritvärde</div>
            </th>
            </thead>
            <tbody>
            {% for id in result %}
                {% if id.is_visible %}
                <tr {% if id.status == "Aktiv" %} class="table-primary" {% endif %}>
                <td>
                <a href="{{ id.page_url }}">{{ id.title|default:_"Untitled" }}</a>
                {% if id.skolenhet.Huvudman.Typ == "Enskild" %}
                {% if id.o.huvudman.o.i_koncern.title %}
                 ( {{ id.o.huvudman.o.i_koncern.title }} )
                {% elseif id.o.huvudman.is_a.koncern %}
                 ( {{ id.o.huvudman.title }} )
                {% endif %}
                {% endif %}
                </td>
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
        {% elseif id.statistics.type == "gy" %}
        {{ id.skolenhet.Skolformer|gy_programs }}
        {% endif %}
        </td>
    </tr>
    {% endif %}
            {% endfor %}
            </tbody>
            </table>

        </div>
        {% with qpayload|qargs_filter|join:"," as qpp %}
        {% pager dispatch="none"
             result=result
             topic="model/location/post/push"
             qpayload=qpp
             hide_single_page
        %}
        {% endwith %}
    {% endwith %}
{% endwith %}
