{% if id.is_visible %}
<div class="list-item{% if is_highlight or id.is_featured %} featured{% endif %}">
    <p class="title">
      {% if id.status == "Aktiv" or not id.status %}
        <a href="{{ id.page_url }}">
            {{ id.title|default:_"Untitled" }}
        </a>
      {% else %}
         {{ id.title|default:_"Untitled" }}
          &ndash; {{ id.status }}
      {% endif %}
    </p>
    <p>
        {{ id|summary:120 }}
    </p>
</div>
{% endif %}
