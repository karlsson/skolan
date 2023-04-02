{% if id.is_visible %}
<div class="list-item">
    <p class="title">
        <a href="{{ id.page_url }}">
            {{ id.title|default:_"Untitled" }}
            {% if is_show_cat %}
                <em>&ndash; {{ id.category_id.title }}</em>
            {% endif %}
        </a>&nbsp;
        <span role="button" class="badge bg-primary rounded-pill"
           onClick="$('#coll_{{id}}').collapse('toggle')"
           title="Antal aktiva skolenheter" >
           {{ id.no_of_school_units }}
        </span>
    <div class="collapse" id="coll_{{id}}">
    <ul class="list-group">
    {% if id.s.i_koncern as huvudmen %}
    {% for id in huvudmen|sort:['desc','no_of_school_units'] %}
    <li class="list-group-item">
        <a href="{{ id.page_url}}"><strong>{{ id.title }}</strong></a>
           <span role="button" class="badge bg-primary rounded-pill"
              onClick="$('#coll_{{id}}').collapse('toggle')"
              title="Antal aktiva skolenheter" >
              {{ id.no_of_school_units }}
           </span>
           <div class="collapse" id="coll_{{id}}">
           <ul>
           {% for su in id.s.huvudman|sort:['title'] %}
              <li>
                  {% catinclude "_list_item.tpl" su %}
              </li>
           {% endfor %}
           </ul>
           </div>
        </li>
    {% endfor %}
    {% endif %}
    {% if id.s.huvudman as schoolunits %}
      <li class="list-group-item">
        <ul>
        {% for su in schoolunits|sort:['title'] %}
          <li>
            {% catinclude "_list_item.tpl" su %}
          </li>
        {% endfor %}
        </ul>
      </li>
    {% endif %}
    </ul>
    </div>
    {% if id|summary  %}
       <p>{{ id|summary:120 }}</p>
    {% endif %}
</div>
{% endif %}
