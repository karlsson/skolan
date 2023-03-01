<ul class="nav nav-stacked">
  <li class="divider">Status
    <form id="facet" label="Status" method="get" >
    {% with m.search.facets|facet_part as fvs %}
    {# {% print fvs %} #}
    {% for su_status in fvs.su_status.counts %}
    <div class="form-check">
         <input class="form-check-input" type="checkbox" value="{{ su_status.value }}" id="su_status_{{ su_status.label }}" {% if su_status.label|is_checked:"qfacet.su_status":m.req.qs %} checked {% endif %} onchange="this.form.submit()" name="qfacet.su_status">
         <label class="form-check-label" for="su_status_{{ su_status.label }}">
                {{ su_status.value }} ({{ su_status.count }})
         </label>
    </div>
    {% endfor %}
    {% endwith %}
    </form>
  </li>
</ul>