{% extends "page.tpl" %}

{% block content_after %}
   {% if m.rsc[id].is_cat.koncern %}
       <p>Företaget är ett Koncernmoderbolag.</p>
   {% endif %}

   {% if id.no_of_school_units|is_defined %}
       <p><a href="https://www.merinfo.se/search?who={{ id.name|replace:['org'] }}" target="_blank">merinfo.se</a></p>
       <p>Antal aktiva skolenheter: {{ id.no_of_school_units }}</p>
   {% endif %}

   {% if id.weightedStudentsPerTeacherQuotaGr|is_defined or
      id.weightedStudentsPerTeacherQuotaGy|is_defined %}
      <p>Viktat antal elever per lärare, medeltal:<br/>
   {% if id.weightedStudentsPerTeacherQuotaGr|is_defined %}
          - grundskola: {{ id.weightedStudentsPerTeacherQuotaGr|format_sefloat }}</br>
   {% endif %}
   {% if id.weightedStudentsPerTeacherQuotaGy|is_defined %}
       - gymnasium: {{ id.weightedStudentsPerTeacherQuotaGy|format_sefloat }}
   {% endif %}
      </p>
   {% endif %}
    <div class="page-relations">
        {% if id.s.i_koncern|is_visible as i_koncern %}
            <div class="connections">
                <h3>Dotterbolag</h3>
                <div class="list-items">
                    {% for id in i_koncern|sort:['title'] %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

        {% if id.o.i_koncern|is_visible as i_koncern %}
            <div class="connections">
                <h3>Ingår i</h3>
                <div class="list-items">
                    {% for id in i_koncern %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

        {% if id.s.huvudman|is_visible as huvudman %}
            <div class="connections">
                <h3>Skolenheter</h3>
                <div class="list-items">
                    {% for id in huvudman|sort:['title'] %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

        {% if id.s.owns|is_visible as owns %}
            <div class="connections">
                    <h3>{_ Owner _}</h3>
                <div class="list-items">
                    {% for id in owns %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

        {% if id.o.owns|is_visible as owns %}
            Äger
            <dl class="connections">
                {% for id in owns %}
                    <dt><a href="{{ id.page_url }}">{{ id.title }}</a></dt>
                    <dd class="do_clickable">
                        {{ id|summary:160 }}
                        <a href="{{ id.page_url }}"></a>
                    </dd>
                {% endfor %}
            </dl>
        {% endif %}

        {% if id.s.lobbyist|is_visible as lobbyist %}
            <div class="connections">
                <h3>Lobbyister</h3>
                <div class="list-items">
                    {% for id in lobbyist|sort:['title'] %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

        {% if id.o.lobbyist|is_visible as lobbyist %}
            <div class="connections">
                <h3>Lobbar åt</h3>
                <div class="list-items">
                    {% for id in lobbyist|sort:['title'] %}
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

        {% if id.s.i_styrelse|is_visible as styrelsemedlem %}
            <div class="connections">
                {% with m.search[{latest cat="predicate" name="i_styrelse"}]|is_visible as medlempred %}
                    <h3>Styrelsemedlemmar</h3>
                {% endwith %}
                <div class="list-items">
                    {% for id in styrelsemedlem %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

        {% if id.o.i_styrelse|is_visible as styrelsemedlem %}
            <div class="connections">
                {% with m.search[{latest cat="predicate" name="i_styrelse"}]|is_visible as medlempred %}
                    <h3>{{ medlempred.title }}</h3>
                {% endwith %}
                <div class="list-items">
                    {% for id in styrelsemedlem %}
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
                {% with m.search[{latest cat="predicate" name="about"}]|is_visible as relpred %}
                    <h3>{{ relpred.title }}</h3>
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
