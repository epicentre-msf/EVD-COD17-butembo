# Bubble map of case counts, residence vs notification toggle. Ported from
# evd-2026-app's linelist Place module (R/mod_place.R) - mapgl instead of
# leaflet - trimmed to a single active admin level (no multi-level focus
# ladder, no MSF pins, no PNG export, no pie disaggregation): this dashboard
# covers one health-zone cluster, not a whole country.

`%||%` <- function(x, y) if (is.null(x)) y else x

BOUNDARY_LEVELS <- c("Province", "Health Zone", "Health Area")
MAP_BUBBLE_COL <- "#c10000"
SITE_PIN_ID <- "site_pin"
SITE_PIN_PNG <- "www/img/site_pin.png"
SITE_PIN_H <- 40
BUBBLE_R_MIN <- 3
BUBBLE_R_MAX <- 22
OVERLAY_BOX_STYLE <- paste(
  "background: rgba(255,255,255,0.92); padding: 6px 10px;",
  "border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.2);"
)

mod_map_place_ui <- function(id, full_screen = TRUE) {
  ns <- NS(id)

  pkg_deps <- c("sf", "mapgl")
  if (!rlang::is_installed(pkg_deps)) {
    rlang::check_installed(pkg_deps, reason = "to use the map module.")
  }

  bslib::card(
    full_screen = full_screen,
    bslib::card_header(
      class = "d-flex align-items-center",
      tags$span(class = "fw-semibold me-3", "Place"),
      tags$div(class = "me-auto", uiOutput(ns("footer"))),
      tags$div(
        class = "d-flex align-items-center gap-2",
        shiny::selectizeInput(
          ns("level"),
          label = NULL,
          choices = BOUNDARY_LEVELS,
          selected = "Health Area",
          width = "170px",
          options = list(openOnFocus = FALSE)
        ),
        shiny::selectizeInput(
          ns("location_type"),
          label = NULL,
          choices = c("Residence" = "res", "Notification" = "notif"),
          selected = "res",
          width = "170px",
          options = list(openOnFocus = FALSE)
        )
      )
    ),
    bslib::card_body(
      padding = 0,
      htmltools::div(
        style = "position: relative; flex: 1 1 auto; min-height: 0;",
        mapgl::maplibreOutput(ns("map"), height = "100%"),
        htmltools::div(
          style = paste(
            "position: absolute; bottom: 12px; left: 12px; z-index: 5;"
          ),
          htmltools::div(
            class = "map-overlay-box",
            style = paste(OVERLAY_BOX_STYLE, "font-size: 0.78rem;"),
            shiny::uiOutput(ns("size_legend"))
          )
        )
      )
    )
  )
}

mod_map_place_server <- function(
  id,
  df,
  geo_data_res,
  geo_data_notif,
  facilities = NULL,
  time_filter = shiny::reactiveVal(),
  filter_reset = shiny::reactiveVal(),
  filter_info = shiny::reactiveVal()
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    suppressMessages(sf::sf_use_s2(FALSE))

    # static facility markers - only the rows with lon/lat filled in show up
    sites_sf <- NULL
    if (!is.null(facilities)) {
      f <- dplyr::filter(facilities, !is.na(lon), !is.na(lat))
      if (nrow(f) > 0) {
        sites_sf <- f |>
          dplyr::mutate(tooltip_html = tt_site_html(site)) |>
          sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
      }
    }

    geo_sets <- list(res = geo_data_res, notif = geo_data_notif)
    for (gd in geo_sets) {
      lvls <- purrr::map_chr(gd, "layer_name")
      if (!all(BOUNDARY_LEVELS %in% lvls)) {
        cli::cli_abort(
          "geo_data must contain layers named {.val {BOUNDARY_LEVELS}}."
        )
      }
    }

    active_geo <- reactive({
      lvl <- input$level %||% "Health Area"
      loc <- input$location_type %||% "res"
      gd <- geo_sets[[loc]]
      gd[[which(purrr::map_chr(gd, "layer_name") == lvl)[1]]]
    })

    dcol <- reactive(unname(active_geo()$join_by))

    df_mod <- reactive({
      d <- epishiny:::force_reactive(df)
      tf <- time_filter()
      if (length(tf)) {
        d <- dplyr::filter(
          d,
          dplyr::between(.data[[tf$date_var]], tf$from, tf$to)
        )
      }
      d
    })

    counts <- reactive({
      df_mod() |>
        dplyr::filter(!is.na(.data[[dcol()]])) |>
        dplyr::rename(pcode = dplyr::all_of(dcol())) |>
        dplyr::summarise(total = dplyr::n(), .by = pcode)
    })

    geo_sf <- reactive({
      gl <- active_geo()
      n_before <- nrow(gl$sf)
      d <- gl$sf |>
        dplyr::left_join(
          counts(),
          by = dplyr::join_by(pcode),
          relationship = "one-to-one"
        ) |>
        dplyr::mutate(
          name = .data[[gl$name_var]],
          total = dplyr::coalesce(total, 0L),
          tooltip_html = tt_html(.data$name, .data$total)
        )
      stopifnot(nrow(d) == n_before)
      d
    })

    polys <- reactive({
      dplyr::select(geo_sf(), pcode, name, total, tooltip_html)
    })

    map_bubbles <- reactive({
      geo_sf() |>
        sf::st_drop_geometry() |>
        dplyr::filter(total > 0) |>
        dplyr::mutate(indicator_value = as.numeric(total)) |>
        dplyr::arrange(dplyr::desc(indicator_value)) |>
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
    })

    output$map <- mapgl::renderMaplibre({
      init_geo <- isolate(polys())
      init_bubbles <- isolate(map_bubbles())
      init_level <- isolate(input$level %||% "Health Area")

      m <- suppressWarnings(max(init_bubbles$indicator_value, na.rm = TRUE))
      if (!is.finite(m) || m <= 0) {
        m <- 1
      }

      ls <- admin_line_style(match(init_level, BOUNDARY_LEVELS))

      map <- mapgl::maplibre(
        style = mapgl::carto_style("voyager"),
        bounds = focus_bbox,
        attributionControl = FALSE
      ) |>
        mapgl::add_source(id = "geo", data = init_geo) |>
        mapgl::add_fill_layer(
          id = "geo_fill",
          source = "geo",
          fill_color = "#ff7f0e",
          fill_opacity = hover_opacity_expr()
        ) |>
        mapgl::add_line_layer(
          id = "geo_border",
          source = "geo",
          line_color = ls$color,
          line_width = ls$width,
          line_opacity = ls$opacity
        ) |>
        mapgl::add_line_layer(
          id = "geo_hl",
          source = "geo",
          line_color = "#000000",
          line_width = 3,
          line_opacity = 1,
          filter = list("==", "pcode", "__none__")
        ) |>
        mapgl::add_source(id = "bubbles_src", data = init_bubbles) |>
        mapgl::add_circle_layer(
          id = "bubbles",
          source = "bubbles_src",
          circle_color = MAP_BUBBLE_COL,
          circle_opacity = 0.70,
          circle_stroke_color = "#ffffff",
          circle_stroke_width = 1.2,
          circle_radius = bubble_radius_expr(m)
        ) |>
        mapgl::add_circle_layer(
          id = "bubble_highlight",
          source = "bubbles_src",
          circle_color = "#000000",
          circle_opacity = 0,
          circle_stroke_color = "#000000",
          circle_stroke_width = 2.5,
          circle_radius = bubble_radius_expr(m),
          filter = list("==", "pcode", "__none__")
        )

      if (!is.null(sites_sf)) {
        map <- map |>
          mapgl::add_image(SITE_PIN_ID, SITE_PIN_PNG, pixel_ratio = 2) |>
          mapgl::add_source(id = "sites_src", data = sites_sf) |>
          mapgl::add_symbol_layer(
            id = "sites",
            source = "sites_src",
            icon_image = SITE_PIN_ID,
            icon_anchor = "bottom",
            icon_size = 32 / SITE_PIN_H,
            icon_allow_overlap = TRUE,
            icon_ignore_placement = TRUE
          )
      }

      map <- promote_pcode_ids(map, "geo")
      htmlwidgets::onRender(map, hover_js(ns))
    })

    observe({
      d_geo <- polys()
      d_bub <- map_bubbles()
      m <- suppressWarnings(max(d_bub$indicator_value, na.rm = TRUE))
      if (!is.finite(m) || m <= 0) {
        m <- 1
      }
      proxy <- mapgl::maplibre_proxy("map")
      mapgl::set_source(proxy, "geo_fill", d_geo)
      mapgl::set_source(proxy, "bubbles", d_bub)
      mapgl::set_paint_property(
        proxy,
        "bubbles",
        "circle-radius",
        bubble_radius_expr(m)
      )
      mapgl::set_paint_property(
        proxy,
        "bubble_highlight",
        "circle-radius",
        bubble_radius_expr(m)
      )
    }) |>
      bindEvent(polys(), map_bubbles(), ignoreInit = TRUE)

    observeEvent(
      input$level,
      {
        ls <- admin_line_style(match(input$level, BOUNDARY_LEVELS))
        proxy <- mapgl::maplibre_proxy("map")
        mapgl::set_paint_property(proxy, "geo_border", "line-color", ls$color)
        mapgl::set_paint_property(proxy, "geo_border", "line-width", ls$width)
        mapgl::set_paint_property(
          proxy,
          "geo_border",
          "line-opacity",
          ls$opacity
        )
      },
      ignoreInit = TRUE
    )

    region_select <- reactiveVal(NULL)

    observe(region_select(NULL)) |>
      bindEvent(filter_reset(), ignoreInit = TRUE)

    # a level/location change invalidates a selection: its pcode belonged to
    # a different join column
    observe(region_select(NULL)) |>
      bindEvent(input$level, input$location_type, ignoreInit = TRUE)

    observeEvent(
      input$map_click_scoped,
      {
        feat <- input$map_click_scoped
        if (is.null(feat) || is.null(feat$pcode)) {
          region_select(NULL)
          return()
        }
        sel <- region_select()
        if (!is.null(sel) && identical(sel$id, feat$pcode)) {
          region_select(NULL)
        } else {
          region_select(list(id = feat$pcode, name = feat$name))
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    observeEvent(
      region_select(),
      {
        sel <- region_select()
        tgt <- if (!is.null(sel)) sel$id else "__none__"
        proxy <- mapgl::maplibre_proxy("map")
        mapgl::set_filter(proxy, "geo_hl", list("==", "pcode", tgt))
        mapgl::set_filter(proxy, "bubble_highlight", list("==", "pcode", tgt))
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    missing_text <- reactive({
      d <- df_mod()
      n_missing <- sum(is.na(d[[dcol()]]))
      if (n_missing == 0) {
        return(NULL)
      }
      pcnt_missing <- n_missing / nrow(d)
      lab <- glue::glue(
        "{scales::number(n_missing)} ({scales::percent(pcnt_missing, accuracy = .1)})"
      )
      loc_lab <- if (identical(input$location_type, "notif")) {
        "notification"
      } else {
        "residence"
      }
      glue::glue(
        "Missing {tolower(active_geo()$layer_name)} of \\
         {loc_lab} for {lab} patients"
      )
    })

    output$footer <- renderUI({
      req(missing_text())
      tags$div(
        class = "card-disclaimer",
        HTML('<i class="fa fa-exclamation-triangle"></i>'),
        missing_text()
      )
    })

    output$size_legend <- shiny::renderUI({
      d <- map_bubbles()
      m <- suppressWarnings(max(d$indicator_value, na.rm = TRUE))
      if (!is.finite(m) || m <= 0) {
        m <- 1
      }
      brks <- pretty(c(0, m), n = 3)
      brks <- brks[brks > 0]
      if (length(brks) > 3) {
        brks <- brks[c(1, ceiling(length(brks) / 2), length(brks))]
      }
      vals <- if (length(brks) == 0) m else brks
      radii <- bubble_radius_px(vals, m)
      htmltools::tagList(
        htmltools::div(
          style = "font-weight: 600; margin-bottom: 4px;",
          "Cases"
        ),
        htmltools::div(
          style = "display: flex; gap: 14px; align-items: flex-end;",
          purrr::map2(vals, radii, function(v, r) {
            htmltools::div(
              style = "display: flex; flex-direction: column; align-items: center;",
              htmltools::div(
                style = sprintf(
                  "width: %fpx; height: %fpx; background: %s; opacity: 0.70; border: 1px solid #fff; border-radius: 50%%;",
                  r * 2,
                  r * 2,
                  MAP_BUBBLE_COL
                )
              ),
              htmltools::div(
                style = "margin-top: 3px;",
                format(v, big.mark = " ", scientific = FALSE)
              )
            )
          })
        )
      )
    })

    shiny::reactive({
      sel <- region_select()
      if (is.null(sel)) {
        return(NULL)
      }
      list(
        region_select = sel$id,
        geo_col = dcol(),
        level_name = active_geo()$layer_name,
        region_name = sel$name
      )
    })
  })
}

# =============================================================================
# Helpers (adapted from evd-2026-app's R/mod_place.R and R/utils.R)
# =============================================================================

bubble_radius_expr <- function(m) {
  list(
    "case",
    list("<=", list("get", "indicator_value"), 0),
    0,
    list(
      "interpolate",
      list("linear"),
      list("sqrt", list("get", "indicator_value")),
      0,
      BUBBLE_R_MIN,
      sqrt(m),
      BUBBLE_R_MAX
    )
  )
}

bubble_radius_px <- function(value, m) {
  ifelse(
    value <= 0,
    0,
    BUBBLE_R_MIN + sqrt(value / m) * (BUBBLE_R_MAX - BUBBLE_R_MIN)
  )
}

zoom_interp <- function(...) {
  c(list("interpolate", list("linear"), list("zoom")), list(...))
}

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

hover_opacity_expr <- function(hover_opacity = 0.55) {
  list(
    "case",
    list("boolean", list("feature-state", "hover"), FALSE),
    hover_opacity,
    0
  )
}

tt_html <- function(name, total) {
  glue::glue(
    "<div class='map-tt'>",
    "<div class='map-tt-name'>{name}</div>",
    "<div class='map-tt-count'>n cases: {format(total, big.mark = ' ', scientific = FALSE, trim = TRUE)}</div>",
    "</div>"
  )
}

tt_site_html <- function(site) {
  glue::glue("<div class='map-tt'><div class='map-tt-name'>{site}</div></div>")
}

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

# One rAF-throttled mousemove handler drives tooltip + hover highlight +
# click, all keyed on the feature's `pcode` property.
hover_js <- function(ns) {
  sprintf(
    "function(el, x) {
      var map = el.map;
      if (!map) return;
      var TT_LAYERS = ['sites', 'bubbles', 'geo_fill'];
      function interactable(lid) {
        var lyr = map.getLayer(lid);
        return !!lyr && map.getLayoutProperty(lid, 'visibility') !== 'none';
      }
      var popup = new maplibregl.Popup({
        closeButton: false, closeOnClick: false, maxWidth: '260px'
      });
      var curHover = null;
      function setHover(id) {
        if (curHover === id) return;
        if (curHover !== null) {
          map.setFeatureState({source: 'geo', id: curHover}, {hover: false});
        }
        if (id !== null) {
          map.setFeatureState({source: 'geo', id: id}, {hover: true});
        }
        curHover = id;
      }
      var pending = null;
      function process() {
        var e = pending;
        pending = null;
        if (!e) return;
        var feats;
        try {
          var ids = TT_LAYERS.filter(interactable);
          feats = ids.length ? map.queryRenderedFeatures(e.point, {layers: ids}) : [];
        } catch (err) {
          return;
        }
        var f = feats.length ? feats[0] : null;
        if (f && f.properties && f.properties.tooltip_html) {
          map.getCanvas().style.cursor = 'pointer';
          popup.setLngLat(e.lngLat).setHTML(f.properties.tooltip_html).addTo(map);
        } else {
          map.getCanvas().style.cursor = '';
          popup.remove();
        }
        setHover(f && f.properties && f.properties.pcode != null ? f.properties.pcode : null);
      }
      map.on('mousemove', function(e) {
        var scheduled = pending !== null;
        pending = e;
        if (!scheduled) requestAnimationFrame(process);
      });
      map.on('mouseout', function() {
        pending = null;
        map.getCanvas().style.cursor = '';
        popup.remove();
        setHover(null);
      });
      map.on('click', function(e) {
        var payload = null;
        try {
          var ids = TT_LAYERS.concat(['bubble_highlight']).filter(interactable);
          var feats = ids.length ? map.queryRenderedFeatures(e.point, {layers: ids}) : [];
          if (feats.length) {
            payload = {pcode: feats[0].properties.pcode, name: feats[0].properties.name};
          }
        } catch (err) {
          return;
        }
        Shiny.setInputValue('%s', payload, {priority: 'event'});
      });
    }",
    ns("map_click_scoped")
  )
}
