    {% with m.search::%{ qargs: true,
           'facet.gy_weighted':(q.gyr_weighted == "gy")|if:(['<', q.gyrr_weighted]|stringify):"",
           'facet.gr_weighted':(q.gyr_weighted == "gr")|if:(['<', q.gyrr_weighted]|stringify):"",
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
            <th scope="col">Skolenhet ( Koncern )</th>
            <th scope="col">Status</th>
            <th scope="col">Typ</th>
            <th scope="col">Viktad<br/>elevtäthet</th>
            <th scope="col">Salsa (gr)
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

        {% pager dispatch="none"
             result=result
             topic="model/location/post/push"
             qargs
             gyr_weighted=q.gyr_weighted
             gyrr_weighted=q.gyrr_weighted
             hide_single_page
        %}
   {% endwith %}
