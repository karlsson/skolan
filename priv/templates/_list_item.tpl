{% if id.is_visible %}
<div class="list-item{% if is_highlight or id.is_featured %} featured{% endif %}">
    <p class="title do_clickable">
        <a href="{{ id.page_url }}">
            {{ id.title|default:_"Untitled" }}
            {% if is_show_cat %}
                <em>&ndash; {{ id.category_id.title }}</em>
            {% endif %}
        </a>
    </p>
    {% if id.o.subject as kws %}
    <aside>
    {% for kw in kws %}
        <a role="button" class="btn btn-primary btn-sm" href="{% url page id='article'%}?kw={{kw}}"><span class="glyphicon glyphicon-tag" aria-hidden="true"></span><strong>{{ kw.title }}</strong></a>
        {% if not forloop.last %}&nbsp;{% endif %}
    {% endfor %}
    </aside>
    {% endif %}
     <p>
        {{ id|summary:120 }}
    </p>
    {% if id.is_a.article %}
    <p>{{ id.publication_start | date:"Y-m-d" }}</p>
    {% endif %}
</div>
{% endif %}
