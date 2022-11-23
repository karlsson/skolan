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

        <div class="body">
            {{ id.body|show_media }}
            {% if m.skolan_verket[id.name]| is_visible as statistics %}
            <p>
            <table class="table">
                <thead>
                    <tr>
                    <th scope="col">Skolform</th>
                    <th scope="col">Elever/barn per lärare</th>
                    <th scope="col">Andel behöriga lärare</th>
                    <th scope="col">Bibliotek</th>
                    </tr>
                </thead>
                <tbody>
            {% for id in statistics %}
                    {% with m.skolan_verket.national_values[id.type] as nv %}
                    <tr>
                    <th scope="row">{{ id.type }}</th>
                    <td>
                    {{id.studentsPerTeacherQuota }}
                    {% if nv != "" %}
                      ( {{ nv.studentsPerTeacherQuota }} )
                    {% endif %}
                    </td>
                    <td>
                    {{id.certifiedTeachersQuota}}
                    {% if nv != "" %}
                      ( {{ nv.certifiedTeachersQuota }} )
                    {% endif %}

                    </td>
                    <td>{{id.hasLibrary}}</td>
                    </tr>
                    {% endwith %}
            {% endfor %}
                </tbody>
            </table>
            </p>
            {% if nv != "" %}
            <p>Siffror inom parentes avser riksgenomsnittet</p>
            {% endif %}
            {% endif %}
        </div>
    </article>
{% endblock %}


{% block content_after %}
    <div class="page-relations">
        {% if id.o.huvudman|is_visible as huvudman %}
            <div class="connections">
                <h3>Huvudman</h3>
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
