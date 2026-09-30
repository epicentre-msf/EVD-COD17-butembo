# Health facilities: structures visited before isolation (care pathway).
# Top structures table, visit distributions, map and per-case timeline, all
# driven by the filtered linelist and the long hf_visits table.
# Ported from R/10_health-facilitiesf.R and R/epishiny_timeline.R; the timeline
# labels cases by unique_id only, as no patient names reach the app.

source("fn_facilities.R")

HF_CASE_RAMP <- c("#ffffff", "#fd7e14")
HF_MAP_RAMP <- c("#d7301f", "#7f0000")
HF_TL_DISEASE <- "#f2b0b0"
HF_TL_ONSET <- "darkred"
HF_TL_RECOVERED <- "#add8e6"
HF_TL_DIED <- "#7f7f7f"

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
    highcharter::hc_yAxis(title = list(text = "Count"), allowDecimals = FALSE) |>
    highcharter::hc_plotOptions(
      column = list(pointPadding = 0, groupPadding = 0.02, borderWidth = 1)
    ) |>
    highcharter::hc_add_series(
      data = counts,
      color = "#3a7ca5",
      name = "Count",
      tooltip = list(pointFormat = "<b>{point.y}</b>")
    ) |>
    highcharter::hc_legend(enabled = FALSE)
}

mod_facilities_ui <- function(id) {
  ns <- shiny::NS(id)

  pkg_deps <- c("sf", "mapgl", "reactable", "highcharter", "stringdist")
  if (!rlang::is_installed(pkg_deps)) {
    rlang::check_installed(pkg_deps, reason = "to use the facilities module.")
  }

  htmltools::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      min_height = 520,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Structures that saw the most cases"),
        bslib::card_body(reactable::reactableOutput(ns("top_tbl"))),
        bslib::card_footer(shiny::uiOutput(ns("top_footer")))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Map of the top structures"),
        bslib::card_body(
          padding = 0,
          mapgl::maplibreOutput(ns("map"), height = "100%")
        ),
        bslib::card_footer(shiny::uiOutput(ns("map_footer")))
      )
    ),
    bslib::layout_columns(
      col_widths = c(4, 8),
      min_height = 560,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Visits per case"),
        bslib::card_body(
          bslib::layout_column_wrap(
            width = 1,
            heights_equal = "row",
            highcharter::highchartOutput(ns("dist_n"), height = "100%"),
            highcharter::highchartOutput(ns("dist_los"), height = "100%")
          )
        ),
        bslib::card_footer(shiny::uiOutput(ns("dist_footer")))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex align-items-center gap-3",
          htmltools::tags$span(class = "me-auto", "Care pathway timeline"),
          shiny::selectInput(
            ns("tl_facility"),
            label = NULL,
            choices = character(0),
            width = "220px"
          ),
          shiny::numericInput(
            ns("tl_max"),
            label = NULL,
            value = 30,
            min = 5,
            max = 100,
            step = 5,
            width = "90px"
          )
        ),
        bslib::card_body(
          padding = 0,
          highcharter::highchartOutput(ns("timeline"), height = "100%")
        ),
        bslib::card_footer(shiny::uiOutput(ns("tl_footer")))
      )
    )
  )
}

mod_facilities_server <- function(id, df, hf_visits, hf_geo, adm2, adm3) {
  shiny::moduleServer(id, function(input, output, session) {
    suppressMessages(sf::sf_use_s2(FALSE))

    # FOSA layer and structure -> layer row lookup depend on no filter: build once
    hf_ref <- hf_geo |>
      sf::st_transform(4326) |>
      hf_prepare_ref()

    hf_lookup <- hf_visits |>
      dplyr::filter(!is.na(hf_name)) |>
      dplyr::distinct(hf_name, hf_as) |>
      dplyr::mutate(
        ref_row = purrr::map2_int(
          hf_name,
          hf_as,
          \(x, y) hf_match_idx(x, y, hf_ref)
        )
      )

    adm2_4326 <- sf::st_transform(adm2, 4326)
    adm3_4326 <- sf::st_transform(adm3, 4326)

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
    top <- shiny::reactive({
      shiny::req(anchor())
      hf_top_structures(visits_f(), anchor())
    })

    output$top_tbl <- reactable::renderReactable({
      d <- top()
      shiny::validate(shiny::need(nrow(d) > 0, "No structure visited in the last 21 days."))

      domain <- c(0, max(d$j21))
      count_col <- function(nm) {
        reactable::colDef(name = nm, style = ramp_style(HF_CASE_RAMP, domain))
      }

      reactable::reactable(
        d,
        highlight = TRUE,
        compact = TRUE,
        pagination = FALSE,
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

    output$top_footer <- shiny::renderUI({
      shiny::req(anchor())
      htmltools::div(
        class = "small text-muted",
        paste0(
          "Cases seen in the last 7 / 14 / 21 days to ",
          format(anchor(), "%d %b %Y"),
          ". ",
          paste(HF_CTE_EXCLUDE, collapse = ", "),
          " excluded: transit/treatment centres. ",
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
      d <- dplyr::left_join(
        d,
        hf_lookup,
        by = dplyr::join_by(hf_name, hf_as),
        relationship = "many-to-one"
      )
      stopifnot(nrow(d) == n_top)
      d |>
        dplyr::filter(!is.na(ref_row)) |>
        (\(x) {
          sf::st_sf(
            x[c("hf_name", "hf_as", "hf_zs", "total", "j7", "j14", "j21")],
            geometry = sf::st_geometry(hf_ref)[x$ref_row]
          )
        })() |>
        dplyr::mutate(
          tooltip_html = paste0(
            "<b>", hf_name, "</b><br>", hf_as, " | ", hf_zs,
            "<br>Total: ", total,
            "<br>7 d: ", j7, " &middot; 14 d: ", j14, " &middot; 21 d: ", j21
          )
        )
    })

    output$map <- mapgl::renderMaplibre({
      pts <- map_points()

      m <- mapgl::maplibre(
        style = mapgl::carto_style("voyager"),
        bounds = sf::st_bbox(adm3_4326),
        attributionControl = FALSE
      ) |>
        mapgl::add_source(id = "adm3", data = adm3_4326) |>
        mapgl::add_line_layer(
          id = "adm3_line",
          source = "adm3",
          line_color = "#9a9a9a",
          line_width = 1
        ) |>
        mapgl::add_source(id = "adm2", data = adm2_4326) |>
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
          circle_radius = 11,
          circle_opacity = 0.92,
          circle_stroke_color = "#ffffff",
          circle_stroke_width = 1.2,
          tooltip = "tooltip_html"
        ) |>
        mapgl::add_symbol_layer(
          id = "hf_total",
          source = "hf",
          text_field = mapgl::get_column("total"),
          text_color = "#ffffff",
          text_size = 12,
          text_allow_overlap = TRUE
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

    output$map_footer <- shiny::renderUI({
      n_unmapped <- nrow(top()) - nrow(map_points())
      htmltools::div(
        class = "small text-muted",
        paste0(
          "Colour by total cases. ",
          n_unmapped,
          " structure(s) not located (informal or absent from the FOSA layer)."
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
        class = "small text-muted",
        paste0(
          nrow(v), " visits by ", dplyr::n_distinct(v$unique_id), " cases; ",
          n_los, " without a valid length of stay."
        )
      )
    })

    #* Timeline ------------------------------------------------------------
    shiny::observe({
      choices <- sort(unique(visits_f()$hf_name))
      # busiest non-CT/CTE structure by default; keep the pick while still valid
      busiest <- visits_f() |>
        dplyr::filter(!hf_name %in% HF_CTE_EXCLUDE) |>
        dplyr::summarise(.by = hf_name, n = dplyr::n_distinct(unique_id)) |>
        dplyr::arrange(dplyr::desc(n)) |>
        dplyr::pull(hf_name)
      current <- shiny::isolate(input$tl_facility)
      selected <- if (!is.null(current) && current %in% choices) {
        current
      } else {
        c(busiest, choices)[1]
      }
      shiny::updateSelectInput(
        session,
        "tl_facility",
        choices = choices,
        selected = selected
      )
    })

    # one row per case; unique_id is minted per case in prep
    cases_tl <- shiny::reactive({
      df() |>
        dplyr::distinct(unique_id, .keep_all = TRUE) |>
        dplyr::semi_join(visits_f(), by = dplyr::join_by(unique_id)) |>
        dplyr::transmute(
          unique_id,
          label = paste0(
            unique_id,
            " (", substr(as.character(sex), 1, 1), round(age), ")"
          ),
          outcome = as.character(type_of_exit),
          date_onset = as.Date(date_symptom_onset),
          date_exit = as.Date(date_exit_eff)
        )
    })

    tl_data <- shiny::reactive({
      cases <- cases_tl() |>
        dplyr::filter(!is.na(date_onset))
      v <- visits_f()

      # waits for the picker to be populated with its default structure
      fac <- shiny::req(input$tl_facility)
      keep <- v$unique_id[v$hf_name == fac]
      cases <- dplyr::filter(cases, unique_id %in% keep)

      n_max <- input$tl_max
      if (is.null(n_max) || is.na(n_max) || n_max < 1) {
        n_max <- 30
      }
      cases <- cases |>
        dplyr::arrange(dplyr::desc(date_onset)) |>
        dplyr::slice_head(n = n_max) |>
        dplyr::arrange(date_onset) |>
        # cases still in care run to the latest notification
        dplyr::mutate(
          end_date = dplyr::coalesce(date_exit, anchor()),
          y = dplyr::row_number() - 1L
        )

      visits <- v |>
        dplyr::filter(!is.na(date_start_HF_visited)) |>
        dplyr::inner_join(
          cases[c("unique_id", "y", "end_date")],
          by = dplyr::join_by(unique_id),
          relationship = "many-to-one"
        ) |>
        dplyr::mutate(
          is_last = date_start_HF_visited == max(date_start_HF_visited),
          .by = unique_id
        ) |>
        dplyr::mutate(
          # +1 so the last day fills its cell
          hf_end = dplyr::case_when(
            !is.na(date_end_HF_visited) &
              date_end_HF_visited >= date_start_HF_visited ~
              date_end_HF_visited + 1,
            is_last ~ end_date + 1,
            .default = date_start_HF_visited + 1
          ),
          # initials, e.g. "CH La Guerison" -> "CLG"
          abbr = purrr::map_chr(
            stringr::str_split(hf_name, "\\s+"),
            \(w) paste(toupper(substr(w, 1, 1)), collapse = "")
          )
        )

      list(cases = cases, visits = visits)
    })

    output$timeline <- highcharter::renderHighchart({
      tl <- tl_data()
      cases <- tl$cases
      shiny::validate(shiny::need(nrow(cases) > 0, "No case to display."))
      visits <- tl$visits

      to_ms <- function(d) as.numeric(as.POSIXct(as.Date(d), tz = "UTC")) * 1000
      fmt <- function(d) format(d, "%d %b %Y")

      bar_pts <- cases |>
        dplyr::transmute(
          y,
          x = to_ms(date_onset),
          x2 = to_ms(end_date + 1),
          color = HF_TL_DISEASE,
          outcome = dplyr::coalesce(outcome, "In care"),
          d_start = fmt(date_onset),
          d_end = dplyr::if_else(is.na(date_exit), "ongoing", fmt(date_exit))
        )

      onset_pts <- cases |>
        dplyr::transmute(
          y,
          x = to_ms(date_onset),
          x2 = to_ms(date_onset + 1),
          color = HF_TL_ONSET,
          d_start = fmt(date_onset)
        )

      exit_pts <- cases |>
        dplyr::filter(!is.na(date_exit)) |>
        dplyr::transmute(
          y,
          x = to_ms(date_exit),
          x2 = to_ms(date_exit + 1),
          color = dplyr::if_else(
            outcome == "Recovered",
            HF_TL_RECOVERED,
            HF_TL_DIED
          ),
          outcome,
          d_end = fmt(date_exit)
        )

      hf_pts <- visits |>
        dplyr::transmute(
          y,
          x = to_ms(date_start_HF_visited),
          x2 = to_ms(hf_end),
          color = "rgba(0,0,0,0)",
          borderColor = "#262626",
          hf_name,
          abbr,
          d_start = fmt(date_start_HF_visited),
          d_end = fmt(hf_end - 1)
        )

      range_start <- min(c(cases$date_onset, visits$date_start_HF_visited))
      range_end <- max(c(cases$end_date + 1, visits$hf_end))

      highcharter::highchart() |>
        highcharter::hc_chart(type = "xrange") |>
        highcharter::hc_xAxis(
          type = "datetime",
          opposite = TRUE,
          min = to_ms(range_start),
          max = to_ms(range_end),
          tickInterval = 7 * 24 * 3600 * 1000,
          labels = list(format = "{value:%e %b}")
        ) |>
        highcharter::hc_yAxis(
          categories = cases$label,
          reversed = TRUE,
          title = list(text = NULL)
        ) |>
        # grouping = FALSE overlays the series on one row instead of stacking
        highcharter::hc_plotOptions(
          xrange = list(pointWidth = 14, borderRadius = 0, grouping = FALSE)
        ) |>
        highcharter::hc_add_series(
          name = "Illness",
          data = highcharter::list_parse(bar_pts),
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Illness ({point.outcome})</b><br>Onset: {point.d_start}<br>Exit: {point.d_end}"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Structures",
          data = highcharter::list_parse(hf_pts),
          dataLabels = list(enabled = TRUE, format = "{point.abbr}"),
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>{point.hf_name}</b><br>{point.d_start} to {point.d_end}"
          )
        ) |>
        # onset and exit last so they stay above the structure rectangles
        highcharter::hc_add_series(
          name = "Onset",
          data = highcharter::list_parse(onset_pts),
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Onset</b><br>{point.d_start}"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Exit",
          data = highcharter::list_parse(exit_pts),
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Exit ({point.outcome})</b><br>{point.d_end}"
          )
        ) |>
        highcharter::hc_legend(enabled = FALSE)
    })

    output$tl_footer <- shiny::renderUI({
      all_cases <- cases_tl()
      n_no_onset <- sum(is.na(all_cases$date_onset))
      htmltools::div(
        class = "small text-muted",
        paste0(
          "Latest ", nrow(tl_data()$cases), " cases by onset, from the ",
          nrow(all_cases), " with a recorded visit; ",
          n_no_onset, " without onset date cannot be drawn. ",
          "Cases still in care run to the latest notification."
        )
      )
    })
  })
}
