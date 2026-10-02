# Health facilities: structures visited before isolation (care pathway).
# Top structures table, visit distributions, map and per-case timeline, all
# driven by the filtered linelist and the long hf_visits table.
# Ported from R/archives/10_health-facilitiesf.R; the timeline is mod_timeline.R.

source("mod_timeline.R")

HF_CASE_RAMP <- c("#ffffff", "#fd7e14")
HF_MAP_RAMP <- c("#d7301f", "#7f0000")

# integer-binned histogram; zero bins kept so gaps in the distribution show
hc_count_bars <- function(x, title, x_lab) {
  x <- x[!is.na(x) & x >= 0]
  shiny::validate(shiny::need(length(x) > 0, "No values to display."))
  lvls <- seq(0, max(x))
  counts <- tabulate(as.integer(x) + 1L, nbins = length(lvls))

  highcharter::highchart() |>
    highcharter::hc_chart(type = "column") |>
    highcharter::hc_title(
      text = title,
      align = "left",
      style = list(fontSize = "13px", fontWeight = "bold")
    ) |>
    highcharter::hc_xAxis(categories = lvls, title = list(text = x_lab)) |>
    highcharter::hc_yAxis(
      title = list(text = "Count"),
      allowDecimals = FALSE
    ) |>
    highcharter::hc_plotOptions(
      column = list(pointPadding = 0, groupPadding = 0.02, borderWidth = 1)
    ) |>
    highcharter::hc_add_series(
      data = highcharter::list_parse(data.frame(
        y = counts,
        pct = sprintf("%.1f%%", 100 * counts / sum(counts))
      )),
      color = "#3a7ca5",
      name = "Count",
      tooltip = list(
        pointFormat = "<b>{point.y}</b> ({point.pct})"
      )
    ) |>
    highcharter::hc_legend(enabled = FALSE)
}

mod_facilities_ui <- function(id) {
  ns <- shiny::NS(id)

  pkg_deps <- c("sf", "mapgl", "reactable", "highcharter")
  if (!rlang::is_installed(pkg_deps)) {
    rlang::check_installed(pkg_deps, reason = "to use the facilities module.")
  }

  bslib::layout_columns(
    col_widths = c(5, 7),
    min_height = 980,
    bslib::navset_card_tab(
      full_screen = TRUE,
      id = ns("hf_tab"),
      title = "Structures that saw cases",
      bslib::nav_panel(
        "Map",
        shiny::uiOutput(ns("map_disclaimer")),
        mapgl::maplibreOutput(ns("map"), height = "100%")
      ),
      bslib::nav_panel(
        "Table",
        shiny::uiOutput(ns("table_disclaimer")),
        reactable::reactableOutput(ns("top_tbl"))
      ),
      bslib::nav_panel(
        "Flows",
        htmltools::div(
          class = "card-disclaimer",
          "Consecutive visits, all cases; not affected by the filters."
        ),
        mapgl::maplibreOutput(ns("flow_map"), height = "100%")
      )
    ),
    bslib::layout_columns(
      col_widths = 12,
      row_heights = c(1, 1.2),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex align-items-center flex-wrap gap-3",
          "Visits per case",
          shiny::uiOutput(ns("dist_footer"))
        ),
        bslib::card_body(
          bslib::layout_column_wrap(
            width = 1 / 2,
            highcharter::highchartOutput(ns("dist_n"), height = "100%"),
            highcharter::highchartOutput(ns("dist_los"), height = "100%")
          )
        )
      ),
      mod_timeline_ui(ns("timeline"))
    )
  )
}

mod_facilities_server <- function(
  id,
  df,
  hf_visits,
  hf_cases,
  flow_locations,
  flow_edges,
  adm2,
  adm3
) {
  shiny::moduleServer(id, function(input, output, session) {
    suppressMessages(sf::sf_use_s2(FALSE))

    # windows end on the latest notification, as the period filter does
    anchor <- shiny::reactive({
      a <- suppressWarnings(max(df()$date_notification, na.rm = TRUE))
      if (is.finite(a)) a else NA
    })

    # visits by the cases left after the sidebar, period and map filters
    visits_f <- shiny::reactive({
      hf_visits |>
        dplyr::filter(!is.na(hf_name)) |>
        dplyr::semi_join(
          dplyr::distinct(df(), unique_id),
          by = dplyr::join_by(unique_id)
        )
    })

    #* Top structures ------------------------------------------------------
    # cases who started a visit in the `n` days up to and including `anchor`
    n_seen <- function(patient, date, anchor, n) {
      idx <- !is.na(date) & date >= anchor - (n - 1) & date <= anchor
      dplyr::n_distinct(patient[idx])
    }

    # structures ranked by cases seen; 7/14/21 d windows are cumulative
    top <- shiny::reactive({
      shiny::req(anchor())
      a <- anchor()
      visits_f() |>
        dplyr::summarise(
          .by = c(raw_name, hf_name, hf_as, hf_zs),
          total = dplyr::n_distinct(unique_id),
          j7 = n_seen(unique_id, date_start_HF_visited, a, 7),
          j14 = n_seen(unique_id, date_start_HF_visited, a, 14),
          j21 = n_seen(unique_id, date_start_HF_visited, a, 21)
        ) |>
        dplyr::arrange(
          dplyr::desc(total),
          dplyr::desc(j21),
          dplyr::desc(j14),
          dplyr::desc(j7)
        )
    })

    output$top_tbl <- reactable::renderReactable({
      # raw_name is only the map's join key
      d <- dplyr::select(top(), -raw_name)
      shiny::validate(shiny::need(nrow(d) > 0, "No structure visited."))

      domain <- c(0, max(d$j21))
      # cell background from the colour ramp, scaled to the 21 d maximum
      ramp_pal <- scales::colour_ramp(HF_CASE_RAMP)
      count_col <- function(nm) {
        reactable::colDef(
          name = nm,
          style = function(value) {
            if (is.null(value) || is.na(value)) {
              return(list())
            }
            frac <- max(
              0,
              min(1, (value - domain[1]) / (domain[2] - domain[1]))
            )
            list(background = ramp_pal(frac))
          }
        )
      }

      reactable::reactable(
        d,
        highlight = TRUE,
        compact = TRUE,
        pagination = FALSE,
        # fixed height makes the body scroll, header stays pinned
        height = 800,
        defaultSorted = list(total = "desc"),
        theme = reactable::reactableTheme(
          style = list(fontSize = "0.82rem"),
          headerStyle = list(fontSize = "0.78rem", fontWeight = 600),
          cellPadding = "4px 6px"
        ),
        defaultColDef = reactable::colDef(align = "center", minWidth = 60),
        columns = list(
          hf_name = reactable::colDef(
            name = "Structure",
            align = "left",
            sticky = "left",
            minWidth = 170
          ),
          hf_as = reactable::colDef(name = "Health area", align = "left"),
          hf_zs = reactable::colDef(name = "Health zone", align = "left"),
          total = reactable::colDef(
            name = "Total",
            style = list(fontWeight = 600),
            minWidth = 70
          ),
          j7 = count_col("7 d"),
          j14 = count_col("14 d"),
          j21 = count_col("21 d")
        )
      )
    })

    top_note <- shiny::reactive({
      shiny::req(anchor())
      htmltools::div(
        class = "card-disclaimer",
        paste0(
          "Cases seen in the last 7 / 14 / 21 days to ",
          format(anchor(), "%d %b %Y"),
          ". ",
          dplyr::n_distinct(visits_f()$unique_id),
          " of ",
          dplyr::n_distinct(df()$unique_id),
          " filtered cases have a recorded visit."
        )
      )
    })

    #* Map -----------------------------------------------------------------
    map_points <- shiny::reactive({
      d <- top()
      n_top <- nrow(d)
      # only structures with coordinates in prep can be drawn
      xy <- hf_cases |>
        dplyr::filter(!is.na(lon)) |>
        dplyr::select(raw_name, lon, lat)
      d <- dplyr::inner_join(
        d,
        xy,
        by = dplyr::join_by(raw_name),
        relationship = "many-to-one"
      )
      stopifnot(nrow(d) <= n_top)
      sf::st_as_sf(d, coords = c("lon", "lat"), crs = 4326) |>
        # largest first, so small dots draw on top
        dplyr::arrange(dplyr::desc(total)) |>
        dplyr::mutate(
          indicator_value = as.numeric(total),
          tooltip_html = paste0(
            "<b>",
            hf_name,
            "</b><br>",
            hf_as,
            " | ",
            hf_zs,
            "<br>Total: ",
            total,
            "<br>7 d: ",
            j7,
            " &middot; 14 d: ",
            j14,
            " &middot; 21 d: ",
            j21
          )
        )
    })

    output$map <- mapgl::renderMaplibre({
      pts <- map_points()

      m <- mapgl::maplibre(
        style = mapgl::carto_style("voyager"),
        bounds = focus_bbox,
        attributionControl = FALSE
      ) |>
        mapgl::add_source(id = "adm3", data = adm3) |>
        mapgl::add_line_layer(
          id = "adm3_line",
          source = "adm3",
          line_color = "#9a9a9a",
          line_width = 1
        ) |>
        mapgl::add_source(id = "adm2", data = adm2) |>
        mapgl::add_line_layer(
          id = "adm2_line",
          source = "adm2",
          line_color = "#333333",
          line_width = 1.8
        )

      if (nrow(pts) == 0) {
        return(m)
      }

      rng <- range(pts$total)
      fill <- if (rng[1] == rng[2]) {
        HF_MAP_RAMP[1]
      } else {
        mapgl::interpolate(
          column = "total",
          values = rng,
          stops = HF_MAP_RAMP
        )
      }

      m |>
        mapgl::add_source(id = "hf", data = pts) |>
        mapgl::add_circle_layer(
          id = "hf_circles",
          source = "hf",
          circle_color = fill,
          circle_radius = bubble_radius_expr(rng[2]),
          circle_opacity = 0.92,
          circle_stroke_color = "#ffffff",
          circle_stroke_width = 1.2,
          tooltip = "tooltip_html"
        ) |>
        # collision handling drops overlapping names rather than stacking them
        mapgl::add_symbol_layer(
          id = "hf_names",
          source = "hf",
          text_field = mapgl::get_column("hf_name"),
          text_size = 10,
          text_offset = c(0, 1.9),
          text_color = "#262626",
          text_halo_color = "#ffffff",
          text_halo_width = 1.5
        )
    })

    #* Flow map ------------------------------------------------------------
    # static: hf_flows has no case key, so the sidebar filters do not reach it
    output$flow_map <- mapgl::renderMaplibre({
      mapgl::maplibre(
        style = mapgl::carto_style("voyager"),
        bounds = focus_bbox,
        attributionControl = FALSE
      ) |>
        mapgl::add_source(id = "adm2", data = adm2) |>
        mapgl::add_line_layer(
          id = "adm2_line",
          source = "adm2",
          line_color = "#333333",
          line_width = 1.8
        ) |>
        mapgl::add_flowmap(
          id = "hf_flows",
          locations = flow_locations,
          flows = flow_edges,
          flow_lines_rendering_mode = "curved",
          flow_location_labels_enabled = TRUE
        )
    })

    output$map_disclaimer <- shiny::renderUI(map_note())
    output$table_disclaimer <- shiny::renderUI(top_note())

    map_note <- shiny::reactive({
      n_unmapped <- nrow(top()) - nrow(map_points())
      n_cases <- dplyr::n_distinct(df()$unique_id)
      n_no_hf <- n_cases - dplyr::n_distinct(visits_f()$unique_id)
      pct_no_hf <- if (n_cases > 0) 100 * n_no_hf / n_cases else NA_real_
      htmltools::div(
        class = "card-disclaimer",
        paste0(
          n_no_hf,
          " of ",
          n_cases,
          " cases (",
          sprintf("%.1f", pct_no_hf),
          "%) have no health facility information. ",
          n_unmapped,
          " of ",
          nrow(top()),
          " facilities not located."
        )
      )
    })

    #* Distributions -------------------------------------------------------
    output$dist_n <- highcharter::renderHighchart({
      v <- visits_f()
      shiny::validate(shiny::need(nrow(v) > 0, "No recorded visits."))

      v |>
        dplyr::summarise(.by = unique_id, value = dplyr::n_distinct(hf_name)) |>
        dplyr::pull(value) |>
        hc_count_bars("Structures visited (per case)", "Structures")
    })

    output$dist_los <- highcharter::renderHighchart({
      v <- visits_f()
      shiny::validate(shiny::need(nrow(v) > 0, "No recorded visits."))

      # negative stays are entry errors, already NA in prep
      v |>
        dplyr::filter(!is.na(los)) |>
        dplyr::pull(los) |>
        hc_count_bars("Length of stay per structure (days)", "Days")
    })

    output$dist_footer <- shiny::renderUI({
      v <- visits_f()
      n_los <- sum(is.na(v$los))
      htmltools::div(
        class = "card-disclaimer",
        paste0(
          nrow(v),
          " visits by ",
          dplyr::n_distinct(v$unique_id),
          " cases; ",
          n_los,
          " without a valid length of stay."
        )
      )
    })

    #* Timeline ------------------------------------------------------------
    # the module reads wide visit slots, so rebuild them from the long table
    tl_df <- shiny::reactive({
      cases <- df() |>
        dplyr::distinct(unique_id, .keep_all = TRUE) |>
        dplyr::transmute(
          unique_id,
          age = round(age),
          # first letter only, so "Male" / "Homme" map to M / H
          sex = substr(as.character(sex), 1, 1),
          type_of_exit = as.character(type_of_exit),
          date_symptom_onset = as.Date(date_symptom_onset),
          date_exit_eff = as.Date(date_exit_eff)
        )

      # slot numbers repeat within a case (e.g. isolation is slot 6), so renumber
      wide <- visits_f() |>
        dplyr::arrange(unique_id, visit) |>
        dplyr::mutate(visit = dplyr::row_number(), .by = unique_id) |>
        dplyr::transmute(
          unique_id,
          visit,
          HF_name_visited = hf_name,
          date_start_HF_visited,
          date_end_HF_visited
        ) |>
        tidyr::pivot_wider(
          names_from = visit,
          names_sep = "",
          values_from = c(
            HF_name_visited,
            date_start_HF_visited,
            date_end_HF_visited
          )
        )

      # inner join: cases without a recorded visit have nothing to draw
      out <- dplyr::inner_join(
        cases,
        wide,
        by = dplyr::join_by(unique_id),
        relationship = "one-to-one"
      )
      stopifnot(nrow(out) <= nrow(cases))
      out
    })

    mod_timeline_server(
      "timeline",
      df = tl_df,
      male_values = c("M", "H")
    )
  })
}
