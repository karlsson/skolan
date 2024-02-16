<form id="facet" label="Status" method="get"
    class="do_forminit"
    data-onsubmit-topic="model/location/post/qlist/submit"
    data-oninput-topic="model/location/post/qlist/submit">
    {% with m.search.facets::%{ qargs: true,
           'facet.gy_weighted':(q.gyr_weighted == "gy")|if:(['<', q.gyrr_weighted]|stringify):"",
           'facet.gr_weighted':(q.gyr_weighted == "gr")|if:(['<', q.gyrr_weighted]|stringify):"",
            cat: id
            } as result %}
    {% with result|facet_part as fvs %}
  <li class="divider">Status
    {# {% print fvs %} #}
    {% for su_status in fvs.su_status.counts %}
    <div class="form-check">
         <input class="form-check-input" type="checkbox" name="qfacet.su_status[]"
         value="{{ su_status.value }}" id="su_status_{{ su_status.label }}">
         <label class="form-check-label" for="su_status_{{ su_status.label }}">
                {{ su_status.label }} - ({{ su_status.count }})
         </label>
    </div>
    {% endfor %}
  </li>
  <li class="divider">Typ av huvudman
    {% for su_typ in fvs.su_typ.counts %}
    <div class="form-check">
         <input class="form-check-input" type="checkbox" name="qfacet.su_typ[]"
         value="{{ su_typ.value }}" id="su_typ_{{ su_typ.label }}">
         <label class="form-check-label" for="su_typ_{{ su_typ.label }}">
                {{ su_typ.label }} - ({{ su_typ.count }})
         </label>
    </div>
    {% endfor %}
  </li>
  <li class="divider">Kommuner
    <div class="form-check">
<select class="form-select" multiple aria-label="multiple select kommun" name="qfacet.kommun[]" >
  <option value="">Välj kommun(er)</option>
    {% for kommun in fvs.kommun.counts|sort_maplist:'label' %}
  <option value="{{ kommun.value }}">
  {{ kommun.label }} - ({{ kommun.count }})
  </option>
    {% endfor %}
</select>
    </div>
  </li>
  <script>
  function ons(selection) {
     var val1 = selection.value;
     var slider = document.getElementById("gyrr_weighted");
     if (val1 == "none"){
        slider.disabled = true;
     } else {
        slider.disabled = false;
     }
  }
  </script>
  <li class="divider">Viktat elever per lärare
  <div class="form-check">
  <label class="input-group-text" for="gyr_weighted">Skolform</label>
  <select class="form-select" id="gyr_weighted" name="gyr_weighted" onchange="ons(this)" >
    <option selected value="none">Ingen</option>
    <option value="gr">Grundskola</option>
    <option value="gy">Gymnasium</option>
  </select>
</div>

  <label for="gyrr_weighted" class="form-label">
    Max {{ q.gyrr_weighted }}
  </label>
  <input type="range" class="form-range" min="0" max="50" step="1"
         id="gyrr_weighted" value="40" name="gyrr_weighted" >
  </input>
  </li>
  <li class="divider">
    <div class="form-check">Enbart de med salsa:
         <input class="form-check-input"
         type="checkbox" value="true" id="is_salsa"
         name="qfacet.is_salsa">
    </div>
  </li>
  <li class="divider">
  <div class="form-group">
    <label for="su_title">Text innehåller</label>
    <input type="text" name="qtext" class="form-control" id="su_title" value="" >
  </div>
  </li>
    {% endwith %}
    {% endwith %}
    </form>
