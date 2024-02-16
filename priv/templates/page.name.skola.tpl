{% extends "page.tpl" %}

{% block content_after %}
        <div id="live2"></div>
        {% live topic="model/location/event/qlist"
           template="_page.name.skola.tpl"
           target="live2"
           method="patch"
           id=id
           qargs
        %}
{% endblock %}
