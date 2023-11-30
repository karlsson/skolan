{% extends "base.tpl" %}

{% block content %}
    <article>
        <h1>{{ id.title }}</h1>
        {% if id.depiction as dep %}
            {% include "_body_media.tpl" id=dep.id size="medium" %}
        {% endif %}

        <p class="summary">
            {{ id.summary }}
        </p>

        {# {% print m.rsc[id] %} #}
        <div class="body">
            {{ id.body|show_media }}
            {% if id.statistics as statistics %}
            <p>
            <table class="table">
                <thead>
                    <tr>
                    <th scope="col">Skolform</th>
                    <th scope="col">Elever/barn per lärare</th>
                    <th scope="col">Andel behöriga lärare</th>
                    <th scope="col">Viktat elever per lärare</th>
                    <th scope="col">Bibliotek</th>
                    {% if statistics|filter:'salsaAverageDev' %}
                    <th scope="col">SALSA Residual för<br/>genomsnittligt meritvärde</th>
                    {% endif %}
                    </tr>
                </thead>
                <tbody>
            {% for id1 in statistics %}
                    {% with m.skolan_verket.national_values[id1.type] as nv %}
                    <tr>
                    <th scope="row"><a href="https://www.skolverket.se/skolutveckling/statistik/sok-statistik-om-forskola-skola-och-vuxenutbildning?sok=SokB&niva=S&nivaval={{ id.skolenhet.Kommun.Kommunkod }}&skola={{ id.name|replace:['se'] }}&vform={{ id1.vform }}&run=1" target="_blank"  title="Skolverkets statistiksidor">{{ id1.type }}</a></th>
                    <td>
                    {{id1.studentsPerTeacherQuota }}
                    {% if nv != "" %}
                      ( {{ nv.studentsPerTeacherQuota }} )
                    {% endif %}
                    </td>
                    <td>
                    {{id1.certifiedTeachersQuota}}
                    {% if nv != "" %}
                      ( {{ nv.certifiedTeachersQuota }} )
                    {% endif %}
                    </td>
                    <td>
                    {{ id1.weightedStudentsPerTeacherQuota|format_sefloat }}
                    {% if nv != "" %}
                      ( {{ nv.weightedStudentsPerTeacherQuota|format_sefloat }} )
                    {% endif %}
                    </td>
                    <td>{{id1.hasLibrary}}</td>
                    {% if id1.salsaAverageDev %}
                    <td>{{ id1.salsaAverageDev }}</td>
                    {% endif %}
                    </tr>
            {% endwith %}
            {% endfor %}
            </tbody>
            </table>
            {% with id.skolenhet.Skolformer|gy_programs as progs %}
            Program/Årskurs: {{ progs }}</br>
              {% for pr in m.skolan_verket.program %}
                {% if (pr.code | member:progs)  %}
                  {{ pr.code }}-{{ pr.name }}&nbsp;&nbsp;
                {% endif %}
              {% endfor %}
            {% endwith %}
            </p>
            {% if nv != "" %}
            <p>Siffror inom parentes avser riksgenomsnittet.</p>
            <p>Viktade världen dubblar elevantalet för andelen obehöriga lärare.
            100*Elever per lärare/(50 + 0,5 * Andel behöriga lärare)</p>
            {% endif %}
            {% endif %}
        </div>
   {% geomap_static longitude=id.location_lng latitude=id.location_lat %}
   <p>{{ id.skolenhet.Besoksadress.Ort }}</p>
   </article>
{% endblock %}


{% block content_after %}
    <div class="page-relations">
        {% if id.o.huvudman|is_visible as huvudman %}
            <div class="connections">
                <h3>Huvudman - {{ id.skolenhet.Huvudman.Typ }}</h3>
                <div class="list-items">
                    {% for id in huvudman %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

        {% if id.s.relation|is_visible as relation %}
            <div class="connections">
                {% with m.search[{latest cat="predicate" name="relation"}]|is_visible as relpred %}
                    <h3>{{ relpred.title }}</h3>
                {% endwith %}
                <div class="list-items">
                    {% for id in relation %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

        {% if id.o.relation|is_visible as relation %}
            <div class="connections">
                {% with m.search[{latest cat="predicate" name="relation"}]|is_visible as relpred %}
                    <h3>{{ relpred.title }}</h3>
                {% endwith %}
                <div class="list-items">
                    {% for id in relation %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

        {% if id.s.about|is_visible as about %}
            <div class="connections">
                {% with m.search[{latest cat="predicate" name="about"}]|is_visible as aboutpred %}
                    <h3>{{ aboutpred.title }}</h3>
                {% endwith %}
                <div class="list-items">
                    {% for id in about %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

    </div>
{% endblock %}
