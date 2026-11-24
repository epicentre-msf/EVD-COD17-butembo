# ! Interactive reference map of North Kivu
# Province, health zones (HZ), health areas (HA) and health facilities (HF)
# from the latest geobase (see update_geobase.R), saved as a self-contained
# html in spatiale/reference_map/, named after the geobase version.
#
# Built with mapgl (MapLibre) to match the "Linelist data (MSF)" place map of
# the evd-2026-app (R/mod_place.R there): Carto Voyager no-labels basemap,
# zoom-graduated admin borders (admin_line_style()), orange hover highlight of
# the interactive unit with its parent zone emboldened, and the same tooltip
# mini-table (.map-tt). Interactivity follows zoom gates: HZ from zoom 6, HA
# from zoom 9 (as in the app), facilities from `hf_min_zoom`.

# SEND_TO_SERVER (--no-send to skip the episerv upload)
source(here::here("R", "cli_args.R"))
source(here::here("R", "0_global.R"))

province <- "Nord-Kivu"

# output: <sharepoint>/spatiale/reference_map/reference_map_<geobase version>.html
# (geobase_version is set in 0_global.R, e.g. COD__ALL__20261116__1234H)
ref_map_dir <- fs::path(
  fs::path_dir(butembo_project_sf_data_path), # spatiale/
  "reference_map"
)
fs::dir_create(ref_map_dir)

ref_map_html <- fs::path(
  ref_map_dir,
  paste0("reference_map_", fs::path_file(geobase_version), ".html")
)

#* Zoom gates (same values as evd-2026-app R/mod_place.R) --------------
HZ_INTERACT_MIN_ZOOM <- 6
HA_INTERACT_MIN_ZOOM <- 9
hf_min_zoom <- 11 # facilities drawn + hoverable from this zoom

#* Styling helpers (from evd-2026-app R/utils.R) -----------------------
# MapLibre linear zoom-interpolation from zoom/value stop pairs
zoom_interp <- function(...) {
  c(list("interpolate", list("linear"), list("zoom")), list(...))
}

# admin border style by depth (1 = Province, 2 = HZ, 3 = HA): darker and
# slightly wider for higher levels; fine meshes fade out when zoomed out
admin_line_style <- function(level_idx) {
  styles <- list(
    list(
      color = "#555555",
      width = zoom_interp(5, 1.2, 7, 1.6, 10, 2.2),
      opacity = 0.9
    ),
    list(
      color = "#666666",
      width = zoom_interp(5, 0.25, 7, 0.6, 9, 0.9, 11, 1.4),
      opacity = zoom_interp(5, 0.35, 7, 0.55, 9, 0.75)
    ),
    list(
      color = "#8a8a8a",
      width = zoom_interp(6, 0.08, 8, 0.3, 10, 0.7, 12, 1),
      opacity = zoom_interp(6, 0.1, 8, 0.35, 10, 0.65)
    )
  )
  styles[[min(max(level_idx, 1), length(styles))]]
}

# boundary fills: invisible at rest (still hit-tested), orange while the
# feature's `hover` feature-state is set by the hover JS
hover_opacity_expr <- function(hover_opacity = 0.55) {
  list(
    "case",
    list("boolean", list("feature-state", "hover"), FALSE),
    hover_opacity,
    0
  )
}

# GeoJSON feature ids = pcode, which the hover JS keys setFeatureState() on
promote_pcode_ids <- function(map, source_ids) {
  map$x$sources <- lapply(map$x$sources, function(src) {
    if (src$id %in% source_ids) {
      src$generateId <- NULL
      src$promoteId <- "pcode"
    }
    src
  })
  map
}

# tooltip mini-table: one row per admin level, parents muted, hovered unit bold
esc <- htmltools::htmlEscape
tt_row <- function(tag, name, value = "", muted = FALSE) {
  cls <- if (muted) "map-tt-parent" else "map-tt-unit"
  paste0(
    "<tr class='",
    cls,
    "'><td><span class='map-tt-lvl'>",
    tag,
    "</span> ",
    esc(name),
    "</td><td>",
    value,
    "</td></tr>"
  )
}
tt_html <- function(...) {
  paste0("<div class='map-tt'><table>", paste0(...), "</table></div>")
}
fmt_pop <- function(x) {
  if_else(
    is.na(x),
    "",
    paste0("pop. ", format(x, big.mark = " ", trim = TRUE))
  )
}

#* Layers ------------------------------------------------------------
# round coordinates to 5 decimals (~1 m): GeoJSON otherwise carries 15+
# digits per coordinate, which more than doubles the html size
round_coords <- function(x, digits = 5) {
  rnd <- function(g) {
    if (is.list(g)) {
      structure(
        rapply(unclass(g), function(m) round(m, digits), how = "replace"),
        class = class(g)
      )
    } else {
      g[] <- round(g, digits)
      g
    }
  }
  st_geometry(x) <- st_sfc(lapply(st_geometry(x), rnd), crs = st_crs(x))
  x
}

nk_prov <- adm1 |>
  filter(adm1_name == province) |>
  simplify_geo() |> # rmapshaper, see utils.R
  round_coords() |>
  transmute(pcode = adm1_pcode, name = adm1_name)

nk_zs <- adm2 |>
  filter(adm1_name == province) |>
  simplify_geo() |> # rmapshaper, see utils.R
  round_coords() |>
  transmute(
    pcode = adm2_pcode,
    name = adm2_name,
    tooltip_html = tt_html(
      tt_row("Province", adm1_name, muted = TRUE),
      tt_row("HZ", adm2_name, fmt_pop(adm2_pop))
    )
  )

nk_as <- adm3 |>
  filter(adm1_name == province) |>
  simplify_geo() |> # rmapshaper, see utils.R
  round_coords() |>
  transmute(
    pcode = adm3_pcode,
    parent_pcode = adm2_pcode, # parent zone, emboldened on hover
    name = adm3_name,
    tooltip_html = tt_html(
      tt_row("Province", adm1_name, muted = TRUE),
      tt_row("HZ", adm2_name, muted = TRUE),
      tt_row("HA", adm3_name, fmt_pop(adm3_pop))
    )
  )

# HZ name labels (the basemap has none), anchored inside each zone
nk_zs_labels <- nk_zs |>
  select(name) |>
  st_set_agr("constant") |>
  st_transform(32735) |>
  st_point_on_surface() |>
  st_transform(4326)

hf_type_labs <- c(
  hospital_reference = "Hôpital général de référence",
  hospital = "Hôpital",
  hc = "Centre de santé",
  hp = "Poste de santé",
  hp_communal = "Poste de santé communal",
  clinic = "Clinique",
  dispensary = "Dispensaire",
  laboratory = "Laboratoire",
  other = "Autre",
  unknown = "Inconnu"
)
hf_type_cols <- setNames(
  RColorBrewer::brewer.pal(length(hf_type_labs), "Paired"),
  unname(hf_type_labs)
)

nk_hf <- readRDS(fs::path(sf_data_path, "COD_hf.rds")) |>
  filter(adm1_name == province) |>
  st_cast("POINT", warn = FALSE) |> # geobase stores single points as MULTIPOINT
  round_coords() |>
  mutate(type = coalesce(unname(hf_type_labs[hf_type]), "Inconnu")) |>
  # raw fields only: the tooltip table is built in the browser (hfTooltip()
  # in hover_js) rather than stored as html on 2k+ points
  transmute(
    name = hf_name,
    type,
    ha = adm3_name,
    hz = adm2_name,
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )

#* JS: hover tooltip + highlight (adapted from evd-2026-app hover_js()) --
# One rAF-throttled mousemove handler: queries the interactive layers
# (topmost first), shows the hovered feature's tooltip_html, sets the orange
# `hover` state on the HA/HZ under the cursor and the `bold` state on its
# parent HZ. A layer is interactive only if visible and past its min zoom.
hover_js <- "
function(el, x) {
  var map = el.map;
  if (!map) return;
  // facility tooltip, same table as the R-built HA/HZ ones (tt_html());
  // exposed on window for the search control
  function esc(s) {
    var d = document.createElement('div');
    d.textContent = (s == null) ? '' : String(s);
    return d.innerHTML;
  }
  // attribute values left unquoted (valid html for plain class names) so
  // this JS needs no quotes that clash with the enclosing R string
  function row(cls, tag, name, val) {
    return '<tr class=' + cls + '><td><span class=map-tt-lvl>' + tag +
      '</span> ' + esc(name) + '</td><td>' + esc(val) + '</td></tr>';
  }
  window.hfTooltip = function(p) {
    return '<div class=map-tt><table>' +
      row('map-tt-parent', 'HZ', p.hz, '') +
      row('map-tt-parent', 'HA', p.ha, '') +
      row('map-tt-unit', 'HF', p.name, '') +
      '<tr class=map-tt-parent><td colspan=2>' + esc(p.type) + '</td></tr>' +
      '<tr class=map-tt-parent><td colspan=2>' +
      Number(p.lat).toFixed(5) + ', ' + Number(p.lon).toFixed(5) +
      '</td></tr></table></div>';
  };
  var TT_LAYERS = ['hf', 'ha_fill', 'hz_fill'];
  function interactable(lid) {
    var lyr = map.getLayer(lid);
    if (!lyr) return false;
    if (map.getLayoutProperty(lid, 'visibility') === 'none') return false;
    var mz = lyr.minzoom;
    return (mz == null) || map.getZoom() >= mz;
  }
  var popup = new maplibregl.Popup({closeButton: false, closeOnClick: false,
                                    maxWidth: '400px', offset: 12});
  var cur = {hz: null, ha: null, bold: null};
  var SRC = {hz: 'hz', ha: 'ha', bold: 'hz'};
  var PROP = {hz: 'hover', ha: 'hover', bold: 'bold'};
  function setState(key, id) {
    if (cur[key] === id) return;
    if (cur[key] !== null) {
      var off = {}; off[PROP[key]] = false;
      map.setFeatureState({source: SRC[key], id: cur[key]}, off);
    }
    if (id !== null) {
      var on = {}; on[PROP[key]] = true;
      map.setFeatureState({source: SRC[key], id: id}, on);
    }
    cur[key] = id;
  }
  var curKey = null; // feature whose html the popup holds
  function clear() {
    curKey = null;
    popup.remove();
    map.getCanvas().style.cursor = '';
    setState('hz', null); setState('ha', null); setState('bold', null);
  }
  var pending = null;
  function process() {
    var e = pending; pending = null;
    if (!e) return;
    var feats;
    try {
      var ids = TT_LAYERS.filter(interactable);
      feats = ids.length ? map.queryRenderedFeatures(e.point, {layers: ids}) : [];
    } catch (err) { return; }
    var f = feats.length ? feats[0] : null;
    if (!f) { clear(); return; }
    map.getCanvas().style.cursor = 'pointer';
    var lid = f.layer.id;
    // rebuild the popup DOM only when the hovered feature changes; otherwise
    // just move it with the cursor
    var key = lid + ':' + (f.id != null ? f.id : f.properties.name + f.properties.lon);
    if (key !== curKey) {
      curKey = key;
      popup.setHTML(lid === 'hf' ? window.hfTooltip(f.properties) : f.properties.tooltip_html);
    }
    popup.setLngLat(e.lngLat);
    if (!popup.isOpen()) popup.addTo(map);
    if (lid === 'ha_fill') {
      setState('ha', f.id); setState('hz', null);
      setState('bold', f.properties.parent_pcode);
    } else if (lid === 'hz_fill') {
      setState('ha', null); setState('hz', f.id); setState('bold', f.id);
    } else {
      setState('ha', null); setState('hz', null); setState('bold', null);
    }
  }
  map.on('mousemove', function(e) {
    if (!pending) requestAnimationFrame(process);
    pending = e;
  });
  map.getCanvas().addEventListener('mouseleave', function() {
    pending = null; clear();
  });
}
"

#* JS: health facility search -------------------------------------------
# Text box with autocomplete (html datalist) on HF labels "name — AS (HZ)";
# picking one flies to it (past the HF zoom gate), opens its tooltip as a
# popup and rings it in red.
hf_search_js <- "
function(el, x, data) {
  var map = el.map;
  if (!map) return;
  function ring(ll) {
    var gj = {type: 'FeatureCollection', features: [{type: 'Feature',
      geometry: {type: 'Point', coordinates: ll}, properties: {}}]};
    if (map.getSource('hf_sel')) { map.getSource('hf_sel').setData(gj); return; }
    map.addSource('hf_sel', {type: 'geojson', data: gj});
    map.addLayer({id: 'hf_sel', type: 'circle', source: 'hf_sel', paint: {
      'circle-radius': 13, 'circle-color': 'rgba(0,0,0,0)',
      'circle-stroke-color': '#d7301f', 'circle-stroke-width': 3}});
  }
  var popup = null;
  var ctrl = {
    onAdd: function() {
      var div = document.createElement('div');
      div.className = 'maplibregl-ctrl maplibregl-ctrl-group hf-search';
      div.innerHTML =
        '<input type=\"search\" list=\"hf-list\" placeholder=\"Rechercher une structure...\">' +
        '<datalist id=\"hf-list\"></datalist>';
      var list = div.querySelector('datalist');
      data.label.forEach(function(l) {
        var o = document.createElement('option'); o.value = l; list.appendChild(o);
      });
      div.querySelector('input').addEventListener('change', function(e) {
        var i = data.label.indexOf(e.target.value);
        if (i < 0) return;
        var ll = [data.lon[i], data.lat[i]];
        ring(ll);
        map.flyTo({center: ll, zoom: 15});
        if (popup) popup.remove();
        popup = new maplibregl.Popup({maxWidth: '400px', offset: 14})
          .setLngLat(ll).setHTML(window.hfTooltip({name: data.name[i],
            type: data.type[i], ha: data.ha[i], hz: data.hz[i],
            lon: data.lon[i], lat: data.lat[i]})).addTo(map);
      });
      return div;
    },
    onRemove: function() {}
  };
  map.addControl(ctrl, 'top-left');
}
"

# tooltip + search box styles (tooltip rules from evd-2026-app www/styles.css)
map_css <- htmltools::tags$style(htmltools::HTML(
  "
.maplibregl-popup:not(:has(.maplibregl-popup-close-button)) .maplibregl-popup-content {
  pointer-events: none; padding: 5px 8px;
}
.map-tt { font-size: .8rem; line-height: 1.25; font-family: sans-serif; }
.map-tt table { border-collapse: collapse; }
.map-tt td { text-align: right; padding: 0 0 0 8px;
  font-variant-numeric: tabular-nums; white-space: nowrap; }
.map-tt td:first-child { text-align: left; padding-left: 0; }
.map-tt-lvl { color: #8d8d8d; font-size: .75em; font-weight: 400; }
.map-tt-parent td { color: #6c757d; }
.map-tt-unit td:first-child { font-weight: 700; }
.hf-search { background: #fff; padding: 4px 6px; }
.hf-search input { width: 280px; border: 0; outline: 0; font: 13px sans-serif; }
"
))

#* Map ---------------------------------------------------------------
ha_style <- admin_line_style(3)
hz_style <- admin_line_style(2)
prov_style <- admin_line_style(1)

nk_bbox <- st_bbox(nk_prov)

ref_map <- mapgl::maplibre(
  style = mapgl::carto_style("positron-no-labels"),
  bounds = nk_bbox,
  # flat map: mapgl defaults to the 3D globe, which is slower to draw and
  # splits the basemap tiles into a mesh that can show as a grid
  projection = "mercator"
) |>
  mapgl::add_source(id = "prov", data = nk_prov) |>
  mapgl::add_source(id = "hz", data = nk_zs) |>
  mapgl::add_source(id = "ha", data = nk_as) |>
  # hit-test / hover fills, coarse to fine so the finest gated level wins
  mapgl::add_fill_layer(
    id = "hz_fill",
    source = "hz",
    fill_color = "#ff7f0e",
    fill_opacity = hover_opacity_expr(),
    min_zoom = HZ_INTERACT_MIN_ZOOM
  ) |>
  mapgl::add_fill_layer(
    id = "ha_fill",
    source = "ha",
    fill_color = "#ff7f0e",
    fill_opacity = hover_opacity_expr(),
    min_zoom = HA_INTERACT_MIN_ZOOM
  ) |>
  # borders, fine to coarse so higher levels draw on top
  mapgl::add_line_layer(
    id = "ha_borders",
    source = "ha",
    line_color = ha_style$color,
    line_width = ha_style$width,
    line_opacity = ha_style$opacity
  ) |>
  mapgl::add_line_layer(
    id = "hz_borders",
    source = "hz",
    line_color = hz_style$color,
    line_width = hz_style$width,
    line_opacity = hz_style$opacity
  ) |>
  mapgl::add_line_layer(
    id = "prov_borders",
    source = "prov",
    line_color = prov_style$color,
    line_width = prov_style$width,
    line_opacity = prov_style$opacity
  ) |>
  # emboldened border of the hovered zone / parent zone of the hovered area
  mapgl::add_line_layer(
    id = "hz_bold",
    source = "hz",
    line_color = "#333333",
    line_width = list(
      "case",
      list("boolean", list("feature-state", "bold"), FALSE),
      2.5,
      0
    ),
    line_opacity = 1
  ) |>
  # health facilities, coloured by type, only once zoomed in
  mapgl::add_circle_layer(
    id = "hf",
    source = nk_hf,
    circle_color = mapgl::match_expr(
      "type",
      values = names(hf_type_cols),
      stops = unname(hf_type_cols),
      default = "#999999"
    ),
    circle_radius = zoom_interp(hf_min_zoom, 3.5, 15, 8),
    circle_stroke_color = "#ffffff",
    circle_stroke_width = 1,
    min_zoom = hf_min_zoom
  ) |>
  # HZ names (the basemap carries no labels), styled like the app's labels
  mapgl::add_symbol_layer(
    id = "hz_labels",
    source = nk_zs_labels,
    text_field = list("get", "name"),
    text_size = zoom_interp(7, 10, 10, 14),
    text_font = list("Open Sans Bold"),
    text_color = "#444444",
    text_halo_color = "#ffffff",
    text_halo_width = 1.8,
    text_allow_overlap = FALSE
  ) |>
  mapgl::add_categorical_legend(
    legend_title = "Structures de santé",
    values = names(hf_type_cols),
    colors = unname(hf_type_cols),
    patch_shape = "circle",
    position = "bottom-left",
    min_zoom = hf_min_zoom
  ) |>
  mapgl::add_layers_control(
    position = "top-right",
    collapsible = TRUE,
    layers = list(
      "Province" = "prov_borders",
      "Zones de santé" = c("hz_borders", "hz_fill", "hz_labels"),
      "Aires de santé" = c("ha_borders", "ha_fill"),
      "Structures de santé" = "hf"
    )
  ) |>
  mapgl::add_navigation_control(
    position = "top-right",
    visualize_pitch = FALSE
  ) |>
  mapgl::add_scale_control(position = "bottom-right", unit = "metric") |>
  mapgl::add_fullscreen_control(position = "top-right") |>
  promote_pcode_ids(c("prov", "hz", "ha"))

hf_search <- nk_hf |>
  st_drop_geometry() |>
  mutate(label = paste0(name, " — AS ", ha, " (", hz, ")")) |>
  arrange(label) |>
  distinct(label, .keep_all = TRUE)

ref_map_widget <- ref_map |>
  htmlwidgets::onRender(hover_js) |>
  htmlwidgets::onRender(
    hf_search_js,
    data = list(
      label = hf_search$label,
      name = hf_search$name,
      type = hf_search$type,
      ha = hf_search$ha,
      hz = hf_search$hz,
      lon = hf_search$lon,
      lat = hf_search$lat
    )
  ) |>
  htmlwidgets::prependContent(map_css)

# preview the final map (with hover + search)
ref_map_widget

#* Save --------------------------------------------------------------
# self-contained html (needs pandoc) so the dashboard can serve a single file
ref_map_widget |>
  htmlwidgets::saveWidget(
    ref_map_html,
    selfcontained = TRUE,
    title = paste("Carte de référence", province)
  )

# saveWidget leaves a <name>_files/ folder behind when it inlines dependencies
ref_map_files_dir <- fs::path_ext_remove(ref_map_html) |>
  paste0("_files")
if (fs::dir_exists(ref_map_files_dir)) {
  fs::dir_delete(ref_map_files_dir)
}

cli::cli_alert_success("Reference map saved to {.path {ref_map_html}}")

#* Dashboard ---------------------------------------------------------
# the "Carte de référence" tab iframes www/reference_map.html (served under
# /assets); fixed name so the app needs no change when the geobase updates.
# www/*.html is git-ignored, so the file reaches episerv by rsync, like
# app_data.rds in 1_prep_data.R.
dashboard_map_html <- here::here(
  "R",
  "butembo_dashboard",
  "www",
  "reference_map.html"
)
fs::file_copy(ref_map_html, dashboard_map_html, overwrite = TRUE)

if (SEND_TO_SERVER) {
  system2(
    "rsync",
    args = c(
      "-zavh",
      fs::path_expand(dashboard_map_html),
      "episerv:/home/epicentre/EVD-COD17-butembo/R/butembo_dashboard/www/"
    )
  )
  cli::cli_alert_success("Reference map sent to episerv")
} else {
  cli::cli_alert_warning("Reference map not sent to episerv (--no-send)")
}
