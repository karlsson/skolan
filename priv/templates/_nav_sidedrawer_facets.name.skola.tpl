<ul class="nav nav-stacked">
    <form id="facet" label="Status" method="get" >
    {% with m.search.facets::%{
           'facet.su_status':m.req.qs|make_filter:'facet.su_status',
           'facet.su_typ':m.req.qs|make_filter:'facet.su_typ',
           'facet.kommun':m.req.qs|make_filter:'facet.kommun',
           'facet.gy_weighted':m.req.qs|make_filter:'gy_weighted':'<'|to_binary,
           'facet.gr_weighted':m.req.qs|make_filter:'gr_weighted':'<'|to_binary,
           'facet.is_salsa':m.req.qs|make_filter:'facet.is_salsa',
            text:q.qsu_title,
            cat: id
    }|facet_part as fvs %}
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
  <script>
  function ons(selection) {
     var val1 = selection.value;
     var slider = document.getElementById("gy_weighted");
     slider.setAttribute("name","q"+val1+"_weighted");
     if (val1 == "none"){
        slider.disabled = true;
     } else {
        slider.disabled = false;
     }
     selection.form.submit();
  }
  </script>
  <li class="divider">Viktat elever per lärare
  <div class="form-check">
  <label class="input-group-text" for="gyr_weighted">Skolform</label>
  <select class="form-select" id="gyr_weighted" name="qgyr_weighted" onchange="ons(this)" >
    <option selected value="none">Ingen</option>
    <option value="gr" {{ (q.qgyr_weighted == "gr")|if:" selected":"" }}>Grundskola</option>
    <option value="gy" {{ (q.qgyr_weighted == "gy")|if:" selected":"" }}>Gymnasium</option>
  </select>
</div>

  <label for="gy_weighted" class="form-label">Max {{ q.qgy_weighted|if:q.qgy_weighted:q.qgr_weighted }}</label>
<input type="range" class="form-range" min="0" max="50" step="1" id="gy_weighted" value="{{ q.qgy_weighted|if:q.qgy_weighted:q.qgr_weighted }}" onchange="this.form.submit()" name="q{{q.qgyr_weighted}}_weighted"  {{ (q.qgyr_weighted == "none")|if:" disabled":"" }} >
  </li>
  <li class="divider">
    <div class="form-check">
         Enbart de med salsa: <input class="form-check-input" type="checkbox" value="true" id="is_salsa" {% if "true"|is_checked:"qfacet.is_salsa":m.req.qs %} checked {% endif %} onchange="this.form.submit()" name="qfacet.is_salsa">
    </div>
  </li>
  <li class="divider">
  <div class="form-group">
    <label for="su_title">Text innehåller</label>
    <input type="text" name="qsu_title" class="form-control" id="su_title" value="{{q.qsu_title}}">
  </div>
  </li>
    {% endwith %}
    </form>
</ul>