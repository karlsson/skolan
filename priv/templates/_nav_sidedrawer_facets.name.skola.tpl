<ul class="nav nav-stacked">
    <form id="facet" label="Status" method="get" >
    {% with m.search.facets::%{
           'facet.su_status':m.req.qs|make_filter:'facet.su_status',
           'facet.su_typ':m.req.qs|make_filter:'facet.su_typ',
           'facet.kommun':m.req.qs|make_filter:'facet.kommun'}|facet_part as fvs %}
  <li class="divider">Status
    {# {% print fvs %} #}
    {% for su_status in fvs.su_status.counts %}
    <div class="form-check">
         <input class="form-check-input" type="checkbox" value="{{ su_status.value }}" id="su_status_{{ su_status.label }}" {% if su_status.value|is_checked:"qfacet.su_status":m.req.qs %} checked {% endif %} onchange="this.form.submit()" name="qfacet.su_status">
         <label class="form-check-label" for="su_status_{{ su_status.label }}">
                {{ su_status.label }} - ({{ su_status.count }})
         </label>
    </div>
    {% endfor %}
  </li>
  <li class="divider">Typ av huvudman
    {% for su_typ in fvs.su_typ.counts %}
    <div class="form-check">
         <input class="form-check-input" type="checkbox" value="{{ su_typ.value }}" id="su_typ_{{ su_typ.label }}" {% if su_typ.value|is_checked:"qfacet.su_typ":m.req.qs %} checked {% endif %} onchange="this.form.submit()" name="qfacet.su_typ">
         <label class="form-check-label" for="su_typ_{{ su_typ.label }}">
                {{ su_typ.label }} - ({{ su_typ.count }})
         </label>
    </div>
    {% endfor %}
  </li>
  <li class="divider">Kommuner
    <div class="form-check">
<select class="form-select" multiple aria-label="multiple select kommun" name="qfacet.kommun" onchange="this.form.submit()" >
    {% for kommun in fvs.kommun.counts|sort_maplist:'label' %}
  <option value="{{ kommun.value }}" {% if kommun.value|is_checked:"qfacet.kommun":m.req.qs %} selected {% endif %}>{{ kommun.label }} - ({{ kommun.count }})</option>
    {% endfor %}
</select>
    </div>
  </li>
    {% endwith %}
    </form>
</ul>