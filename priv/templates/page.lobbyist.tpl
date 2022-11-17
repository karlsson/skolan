{% extends "page.tpl" %}

{% block content_after %}
    <div class="page-relations">
        {% if id.o.works_for|is_visible as works_for %}
            <div class="connections">
	            {% with m.search[{latest cat="predicate" name="works_for"}]|is_visible as wf %}
		            <h3>{{ wf.title }}</h3>
	            {% endwith %}
	            <div class="list-items">
		            {% for id in works_for %}
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
    </div>
{% endblock %}
