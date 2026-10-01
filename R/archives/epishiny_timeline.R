# epishiny.timeline — Shiny module for patient care-pathway timelines
#
# Mirrors the epishiny module pattern (`*_ui` / `*_server`): a bslib card with a
# gear-toggled options sidebar and a highcharter xrange chart. One row per case,
# from symptom onset to exit (recovery / death), with the health structures
# visited drawn as rectangles over the disease phase.
#
# Two filters live in the options sidebar:
#   1) facility — keep only cases that went through a chosen structure
#   2) cases    — choose which patients to display
#
# Requires: shiny, bslib, bsicons, shinyWidgets, highcharter, dplyr, tidyr,
#           stringr, purrr, forcats, glue.

library(shiny)

# ---- UI ----------------------------------------------------------------------

timeline_ui <- function(
  id,
  title = "Chronologie",
  icon = bsicons::bs_icon("activity"),
  tooltip = NULL,
  facility_lab = "Structure de santé",
  cases_lab = "Cas à afficher",
  label_lab = "Identifiant",
  agesex_lab = "Afficher âge / sexe",
  incubation_lab = "Incubation supposée (jours)",
  incubation_days = 8,
  incubation_min = 0,
  incubation_max = 21,
  opts_btn_lab = "Options",
  full_screen = TRUE,
  use_sidebar = TRUE,
  sidebar_title = NULL,
  sidebar_width = 300
) {
  ns <- NS(id)

  pkg_deps <- c("highcharter", "shinyWidgets", "bslib", "bsicons")
  if (!rlang::is_installed(pkg_deps)) {
    rlang::check_installed(pkg_deps, reason = "to use the timeline module.")
  }

  tt <- if (length(tooltip)) {
    bslib::tooltip(
      bsicons::bs_icon(
        "info-circle",
        class = "ms-2 text-primary",
        size = "1.2em"
      ),
      tooltip
    )
  }

  # the two filters that make up the options panel
  inputs_ui <- tagList(
    shinyWidgets::pickerInput(
      ns("facility"),
      facility_lab,
      choices = character(0),
      multiple = FALSE,
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        noneSelectedText = "Toutes les structures"
      )
    ),
    shinyWidgets::pickerInput(
      ns("cases"),
      cases_lab,
      choices = character(0),
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        actionsBox = TRUE,
        selectedTextFormat = "count > 2",
        noneSelectedText = "Aucun cas"
      )
    ),
    shinyWidgets::pickerInput(
      ns("label_field"),
      label_lab,
      choices = character(0),
      multiple = FALSE
    ),
    shinyWidgets::materialSwitch(
      ns("show_agesex"),
      agesex_lab,
      value = TRUE,
      status = "primary"
    ),
    sliderInput(
      ns("incubation"),
      incubation_lab,
      min = incubation_min,
      max = incubation_max,
      value = incubation_days,
      step = 1
    )
  )

  bslib::card(
    full_screen = full_screen,
    bslib::card_header(
      class = "d-flex align-items-center",
      tags$span(icon, title, class = "me-auto pe-2"),
      tt,
      if (!use_sidebar) {
        bslib::popover(
          title = opts_btn_lab,
          id = ns("popover"),
          placement = "left",
          trigger = bsicons::bs_icon(
            "gear",
            title = opts_btn_lab,
            class = "ms-2 text-primary",
            size = "1.2em"
          ),
          inputs_ui
        )
      } else {
        bslib::tooltip(
          actionLink(
            ns("toggle_sidebar"),
            label = bsicons::bs_icon("gear", size = "1.2em"),
            class = "ms-2 text-primary"
          ),
          opts_btn_lab
        )
      }
    ),
    if (use_sidebar) {
      bslib::card_body(
        padding = 0,
        bslib::layout_sidebar(
          padding = 0,
          gap = 0,
          sidebar = bslib::sidebar(
            id = ns("timeline_sidebar"),
            title = sidebar_title,
            width = sidebar_width,
            position = "right",
            open = "closed",
            inputs_ui
          ),
          highcharter::highchartOutput(ns("chart"))
        )
      )
    } else {
      bslib::card_body(
        padding = 0,
        highcharter::highchartOutput(ns("chart"))
      )
    }
  )
}

# ---- SERVER ------------------------------------------------------------------

timeline_server <- function(
  id,
  df,
  id_var = "patient_name",
  name_var = "patient_name",
  pid_var = "pid",
  age_var = "age",
  sex_var = "sex",
  date_onset = "date_symptom_onset",
  date_exit = "date_exit_eff",
  outcome_var = "type_of_exit",
  outcome_levels = c("Guéri", "Décédé"),
  male_values = c("Homme", "M", "Male", "H"),
  hf_name_pattern = "HF_name_visited",
  hf_start_pattern = "date_start_HF_visited",
  hf_end_pattern = "date_end_HF_visited",
  disease_col = "#f2b0b0",
  incubation_col = "#ffe6cc",
  onset_col = "darkred",
  recovered_col = "#add8e6",
  died_col = "#7f7f7f",
  recovered_value = "Guéri"
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # display fields the "Identifiant" picker can switch between
    label_vars <- stats::setNames(c(name_var, pid_var), c("Nom", "ID patient"))

    observeEvent(input$toggle_sidebar, {
      bslib::sidebar_toggle("timeline_sidebar")
    })

    get_df <- reactive({
      if (is.reactive(df)) df() else df
    })

    # label field to display on the y-axis (falls back to the id column) -------
    label_field <- reactive({
      avail <- label_vars[label_vars %in% names(get_df())]
      field <- input$label_field
      if (is.null(field) || !nzchar(field) || !field %in% avail) {
        field <- if (length(avail)) unname(avail[1]) else id_var
      }
      field
    })

    # case-level data: one row per case, onset -> exit -------------------------
    cases_dat <- reactive({
      show_agesex <- isTRUE(input$show_agesex)
      get_df() |>
        dplyr::filter(
          .data[[outcome_var]] %in% outcome_levels,
          !is.na(.data[[date_onset]]),
          !is.na(.data[[date_exit]])
        ) |>
        dplyr::transmute(
          .id = as.character(.data[[id_var]]),
          name = as.character(.data[[label_field()]]),
          age = .data[[age_var]],
          sex = .data[[sex_var]],
          outcome = as.character(.data[[outcome_var]]),
          date_onset = as.Date(.data[[date_onset]]),
          date_exit = as.Date(.data[[date_exit]])
        ) |>
        dplyr::mutate(
          sex_age = paste0(dplyr::if_else(sex %in% male_values, "M", "F"), age),
          label = if (show_agesex) {
            as.character(glue::glue("{name} ({sex_age})"))
          } else {
            name
          },
          onset_end = date_onset + 1,
          exit_end = date_exit + 1
        )
    })

    # visit-level data: one row per structure visited -------------------------
    visits_dat <- reactive({
      long <- get_df() |>
        dplyr::select(
          dplyr::all_of(id_var),
          dplyr::contains(hf_name_pattern),
          dplyr::contains(hf_start_pattern),
          dplyr::contains(hf_end_pattern)
        ) |>
        dplyr::rename(.id = dplyr::all_of(id_var)) |>
        dplyr::mutate(dplyr::across(-.id, as.character)) |>
        tidyr::pivot_longer(
          cols = -.id,
          names_to = c(".value", "visit"),
          names_pattern = "(.*?)(\\d+)$"
        ) |>
        dplyr::rename_with(\(x) stringr::str_remove(x, "_$")) |>
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.character), \(x) {
            dplyr::na_if(stringr::str_squish(x), "")
          })
        ) |>
        dplyr::filter(!is.na(.data[[hf_name_pattern]])) |>
        dplyr::mutate(
          .id = as.character(.id),
          hf_name = stringr::str_squish(
            stringr::str_split_i(
              .data[[hf_name_pattern]],
              stringr::fixed("|"),
              1
            )
          ),
          # initials of each word, e.g. "CH La Guérison" -> "CLG"
          hf_abbr = purrr::map_chr(
            stringr::str_split(hf_name, "\\s+"),
            \(w) {
              stringr::str_c(
                stringr::str_to_upper(stringr::str_sub(w, 1, 1)),
                collapse = ""
              )
            }
          ),
          hf_start = as.Date(.data[[hf_start_pattern]]),
          hf_end_raw = as.Date(.data[[hf_end_pattern]])
        ) |>
        dplyr::filter(!is.na(hf_start))

      # attach case label + exit, then resolve each visit's end date
      long |>
        dplyr::inner_join(
          dplyr::select(cases_dat(), .id, label, exit_end),
          by = ".id"
        ) |>
        dplyr::mutate(
          is_last_visit = hf_start == max(hf_start),
          .by = .id
        ) |>
        # recorded end if valid, else the exit for the last structure, else a
        # single day; +1 so the last day fills its cell
        dplyr::mutate(
          hf_end = dplyr::case_when(
            !is.na(hf_end_raw) & hf_end_raw >= hf_start ~ hf_end_raw + 1,
            is_last_visit ~ exit_end,
            .default = hf_start + 1
          )
        )
    })

    # keep the facility picker in sync with the data ---------------------------
    observe({
      fac_choices <- sort(unique(visits_dat()$hf_name))
      shinyWidgets::updatePickerInput(
        session,
        "facility",
        choices = c("Toutes les structures" = "", fac_choices),
        selected = isolate(input$facility)
      )
    })

    # keep the label-field picker in sync with the available columns -----------
    observe({
      vars <- label_vars[label_vars %in% names(get_df())]
      shinyWidgets::updatePickerInput(
        session,
        "label_field",
        choices = vars,
        selected = isolate(input$label_field) %||% unname(vars[1])
      )
    })

    # cases that passed through the selected facility (all cases if none) ------
    facility_ids <- reactive({
      ids <- cases_dat()$.id
      fac <- input$facility
      if (!is.null(fac) && length(fac) == 1 && nzchar(fac)) {
        ids <- intersect(ids, visits_dat()$.id[visits_dat()$hf_name == fac])
      }
      ids
    })

    # keep the cases picker in sync with the facility filter ------------------
    # when the facility changes we reset the selection to every case in the
    # pool (all structures by default, or all cases of a chosen structure).
    prev_fac <- reactiveVal(NULL)
    observe({
      pool <- cases_dat() |>
        dplyr::filter(.id %in% facility_ids()) |>
        dplyr::arrange(date_onset)
      choices <- stats::setNames(pool$.id, pool$label)

      fac <- input$facility
      facility_changed <- !identical(fac, isolate(prev_fac()))
      prev_fac(fac)

      if (facility_changed) {
        sel <- pool$.id
      } else {
        cur <- isolate(input$cases)
        sel <- intersect(cur, pool$.id)
        if (!length(sel)) {
          sel <- pool$.id
        }
      }

      shinyWidgets::updatePickerInput(
        session,
        "cases",
        choices = choices,
        selected = sel
      )
    })

    # final set of cases to plot ---------------------------------------------
    plot_ids <- reactive({
      sel <- input$cases
      if (is.null(sel) || !length(sel)) {
        return(character(0))
      }
      intersect(facility_ids(), sel)
    })

    # ---- chart -------------------------------------------------------------
    output$chart <- highcharter::renderHighchart({
      ids <- plot_ids()
      shiny::validate(shiny::need(length(ids) > 0, "Aucun cas à afficher"))

      tl <- cases_dat() |>
        dplyr::filter(.id %in% ids) |>
        dplyr::mutate(
          label = forcats::fct_reorder(label, date_onset),
          y = as.integer(label) - 1L
        )
      cats <- levels(tl$label)

      hf <- visits_dat() |>
        dplyr::filter(.id %in% ids) |>
        dplyr::mutate(
          label = factor(label, levels = cats),
          y = as.integer(label) - 1L
        )

      to_ms <- function(d) as.numeric(as.POSIXct(as.Date(d), tz = "UTC")) * 1000
      day_ms <- 24 * 3600 * 1000

      # supposed incubation window: mean days back from symptom onset
      incub_days <- input$incubation
      if (is.null(incub_days) || is.na(incub_days)) {
        incub_days <- 0
      }
      tl <- dplyr::mutate(tl, incub_start = date_onset - incub_days)

      # day-cell grid: gridlines at midnights, labels centred at noon
      range_start <- min(c(tl$incub_start, tl$date_onset, hf$hf_start))
      range_end <- max(c(tl$exit_end, hf$hf_end))
      midnights <- seq(range_start, range_end, by = "day")
      grid_lines <- lapply(
        to_ms(midnights),
        \(v) list(value = v, color = "#d9d9d9", width = 1, zIndex = 1)
      )
      day_labels <- to_ms(utils::head(midnights, -1)) + day_ms / 2

      # month tier: one label centred over each month's visible span
      month_first <- seq(
        as.Date(format(range_start, "%Y-%m-01")),
        range_end,
        by = "month"
      )
      month_next <- seq(
        month_first[1],
        by = "month",
        length.out = length(month_first) + 1
      )[-1]
      seg_start <- pmax(month_first, range_start)
      seg_end <- pmin(month_next, range_end + 1)
      month_labels <- to_ms(seg_start) +
        as.numeric(seg_end - seg_start) * day_ms / 2

      bar_pts <- tl |>
        dplyr::transmute(
          y,
          x = to_ms(date_onset),
          x2 = to_ms(exit_end),
          color = disease_col,
          outcome,
          d_start = format(date_onset, "%d %b %Y"),
          d_end = format(date_exit, "%d %b %Y")
        )

      onset_pts <- tl |>
        dplyr::transmute(
          y,
          x = to_ms(date_onset),
          x2 = to_ms(onset_end),
          color = onset_col,
          d_start = format(date_onset, "%d %b %Y")
        )

      # incubation window drawn before onset (empty when the slider is at 0)
      incub_pts <- tl |>
        dplyr::filter(incub_days > 0) |>
        dplyr::transmute(
          y,
          x = to_ms(incub_start),
          x2 = to_ms(date_onset),
          color = incubation_col,
          d_start = format(incub_start, "%d %b %Y"),
          d_end = format(date_onset, "%d %b %Y")
        )

      exit_pts <- tl |>
        dplyr::transmute(
          y,
          x = to_ms(date_exit),
          x2 = to_ms(exit_end),
          color = dplyr::if_else(
            outcome == recovered_value,
            recovered_col,
            died_col
          ),
          outcome,
          d_end = format(date_exit, "%d %b %Y")
        )

      hf_pts <- hf |>
        dplyr::transmute(
          y,
          x = to_ms(hf_start),
          x2 = to_ms(hf_end),
          color = "rgba(0,0,0,0)",
          borderColor = "#262626",
          hf_name,
          abbr = hf_abbr,
          d_start = format(hf_start, "%d %b %Y"),
          d_end = format(hf_end - 1, "%d %b %Y")
        )

      # French month names for the month tier (day tier is language-neutral)
      month_fmt <- highcharter::JS(
        "function() { var m = ['janvier','février','mars','avril','mai','juin','juillet','août','septembre','octobre','novembre','décembre']; var d = new Date(this.value); return m[d.getUTCMonth()] + ' ' + d.getUTCFullYear(); }"
      )

      highcharter::highchart() |>
        highcharter::hc_chart(type = "xrange") |>
        # two tiers: bare day numbers near the plot, month band above them
        highcharter::hc_xAxis_multiples(
          list(
            type = "datetime",
            opposite = TRUE,
            gridLineWidth = 0,
            tickLength = 0,
            lineWidth = 0,
            min = to_ms(range_start),
            max = to_ms(range_end),
            startOnTick = FALSE,
            endOnTick = FALSE,
            minPadding = 0,
            maxPadding = 0,
            tickPositions = day_labels,
            labels = list(
              format = "{value:%e}",
              style = list(fontSize = "10px", color = "#8c8c8c")
            ),
            plotLines = grid_lines
          ),
          list(
            type = "datetime",
            opposite = TRUE,
            linkedTo = 0,
            gridLineWidth = 0,
            tickLength = 0,
            lineWidth = 0,
            tickPositions = month_labels,
            labels = list(
              formatter = month_fmt,
              style = list(fontWeight = "bold", fontSize = "12px")
            )
          )
        ) |>
        highcharter::hc_yAxis(
          categories = cats,
          reversed = TRUE,
          title = list(text = NULL)
        ) |>
        # grouping = FALSE overlays the series on one row instead of stacking
        highcharter::hc_plotOptions(
          xrange = list(pointWidth = 16, borderRadius = 0, grouping = FALSE)
        ) |>
        highcharter::hc_add_series(
          name = "Maladie",
          data = highcharter::list_parse(bar_pts),
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Maladie ({point.outcome})</b><br>Début : {point.d_start}<br>Sortie : {point.d_end}"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Incubation supposée",
          data = highcharter::list_parse(incub_pts),
          enableMouseTracking = nrow(incub_pts) > 0,
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Incubation supposée</b><br>Du {point.d_start} au {point.d_end}"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Structures",
          data = highcharter::list_parse(hf_pts),
          dataLabels = list(enabled = TRUE, format = "{point.abbr}"),
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>{point.hf_name}</b><br>Du {point.d_start} au {point.d_end}"
          )
        ) |>
        # onset / exit last so they stay on top of the structure rectangles
        highcharter::hc_add_series(
          name = "Début des symptômes",
          data = highcharter::list_parse(onset_pts),
          showInLegend = FALSE,
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Début des symptômes</b><br>{point.d_start}"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Sortie",
          data = highcharter::list_parse(exit_pts),
          showInLegend = FALSE,
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Sortie ({point.outcome})</b><br>{point.d_end}"
          )
        ) |>
        highcharter::hc_legend(enabled = FALSE)
    })

    # expose the current selection for cross-module use
    reactive(plot_ids())
  })
}

# ---- STANDALONE LAUNCHER -----------------------------------------------------
# Minimal app to preview the module, mirroring epishiny::launch_module().

# `...` are passed to timeline_server() (data columns, colours, …); pass
# ui_args = list(...) to override the card / options-panel labels.
launch_timeline <- function(df, ..., ui_args = list()) {
  ui <- bslib::page_fillable(
    shinyjs::useShinyjs(),
    do.call(timeline_ui, c(list("timeline"), ui_args))
  )
  server <- function(input, output, session) {
    timeline_server("timeline", df = df, ...)
  }
  shiny::shinyApp(ui, server)
}
