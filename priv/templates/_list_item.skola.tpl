{% if id.is_visible %}
<div class="list-item{% if is_highlight or id.is_featured %} featured{% endif %}">
    <p class="title do_clickable">
           <a href="{{ id.page_url}}"
           class={% if id.status == 'Aktiv' %}"text-primary"
           {% else %}"text-muted" title="{{id.status}}"
           {% endif %}>{{ id.title }}</a>
    </p>
</div>
{% endif %}
