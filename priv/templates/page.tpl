{% extends "base.tpl" %}

{% block content %}
    <article>
        <aside class="text-right small">
          Upplagd {{ id.publication_start | date:"Y-m-d" }} av {{m.rsc[id.creator_id].title }}
        </aside>
        <h1>{{ id.title }}</h1>
        {% if id.depiction as dep %}
            {% include "_body_media.tpl" id=dep.id size="medium" %}
        {% endif %}

        <p class="summary">
            {{ id.summary }}
        </p>

        <div class="body">
            {{ id.body|show_media }}
        </div>
          {% if id.o.subject as kws %}
            {% for kw in kws %}
            <a role="button" class="btn btn-primary btn-sm" href="{% url page id='article' %}?kw={{kw}}"><span class="glyphicon glyphicon-tag" aria-hidden="true"></span><strong>{{ kw.title }}</strong></a>
            {% if not forloop.last %}&nbsp;{% endif %}
            {% endfor %}
          {% endif %}
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

        {% if id.s.works_for|is_visible as works_for %}
            <div class="connections">
                <h3>Lobbyister</h3>
                <div class="list-items">
                    {% for id in works_for %}
                        {% catinclude "_list_item.tpl" id %}
                    {% endfor %}
                </div>
            </div>
        {% endif %}

        {% if id.o.haspart|is_visible as haspart %}
            <dl class="connections">
                {% for id in haspart %}
                    <dt><a href="{{ id.page_url }}">{{ id.title }}</a></dt>
                    <dd class="do_clickable">
                        {{ id|summary:160 }}
                        <a href="{{ id.page_url }}"></a>
                    </dd>
                {% endfor %}
            </dl>
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

        {% for s in id.s.haspart|is_visible %}
            {% with s.o.haspart|is_visible as siblings %}
                {% for p in s.o.haspart %}
                    {% if p == id %}
                        <p class="page-haspart">
                            {% if siblings[forloop.counter - 1] as prev %}
                                <a class="haspart__prev" href="{{ prev.page_url }}">{{ prev.title }}</a>
                            {% else %}
                                <span></span>
                            {% endif %}
                            <a class="haspart__link" href="{{ s.page_url }}">{{ s.title }}</a>
                            {% if siblings[forloop.counter + 1] as next %}
                                <a class="haspart__next" href="{{ next.page_url }}">{{ next.title }}</a>
                            {% endif %}
                        </p>
                    {% endif %}
                {% endfor %}
            {% endwith %}
        {% endfor %}

        {% if id.o.about|is_visible as about %}
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
