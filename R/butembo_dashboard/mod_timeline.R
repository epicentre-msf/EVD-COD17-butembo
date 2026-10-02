# Care-pathway timeline: one row per case, onset to exit, structures visited
# drawn over the illness bar. Local copy of the epishiny.timeline module, so the
# dashboard carries no dependency on that package.
# Cases are labelled by id only (plus sex / age), as no patient names reach the app.

mod_timeline_ui <- function(
  id,
  title = "Care timeline",
  icon = bsicons::bs_icon("activity"),
  facility_lab = "Health facility",
  cases_lab = "Cases to display",
  agesex_lab = "Show age / sex",
  incubation_lab = "Assumed incubation (days)",
  incubation_days = 8,
  incubation_min = 0,
  incubation_max = 21,
  opts_btn_lab = "Options",
  sidebar_width = 300
) {
  ns <- shiny::NS(id)

  pkg_deps <- c("highcharter", "shinyWidgets")
  if (!rlang::is_installed(pkg_deps)) {
    rlang::check_installed(pkg_deps, reason = "to use the timeline module.")
  }

  inputs_ui <- shiny::tagList(
    shinyWidgets::pickerInput(
      ns("facility"),
      facility_lab,
      choices = character(0),
      multiple = FALSE,
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        noneSelectedText = "All facilities"
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
        noneSelectedText = "No cases"
      )
    ),
    shinyWidgets::materialSwitch(
      ns("show_agesex"),
      agesex_lab,
      value = TRUE,
      status = "primary"
    ),
    shiny::sliderInput(
      ns("incubation"),
      incubation_lab,
      min = incubation_min,
      max = incubation_max,
      value = incubation_days,
      step = 1
    )
  )

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex align-items-center",
      htmltools::tags$span(icon, title, class = "me-auto pe-2"),
      bslib::tooltip(
        shiny::actionLink(
          ns("toggle_sidebar"),
          label = bsicons::bs_icon("gear", size = "1.2em"),
          class = "ms-2 text-primary"
        ),
        opts_btn_lab
      )
    ),
    bslib::card_body(
      padding = 0,
      bslib::layout_sidebar(
        padding = 0,
        gap = 0,
        sidebar = bslib::sidebar(
          id = ns("timeline_sidebar"),
          width = sidebar_width,
          position = "right",
          open = "closed",
          inputs_ui
        ),
        # fill item, not fillable: it takes the card height and scrolls, while
        # the chart inside keeps its pixel height
        bslib::as_fill_item(
          htmltools::tags$div(
            class = "overflow-auto",
            shiny::uiOutput(ns("chart_ui"))
          )
        )
      )
    )
  )
}

# df: one row per case, with the wide HF_name_visited1.. / date_*_HF_visited1..
# slots. `male_values` is matched on sex_var to build the "M45" style label.
mod_timeline_server <- function(
  id,
  df,
  id_var = "unique_id",
  age_var = "age",
  sex_var = "sex",
  date_onset = "date_symptom_onset",
  date_exit = "date_exit_eff",
  outcome_var = "type_of_exit",
  outcome_levels = c("Recovered", "Died"),
  male_values = c("Male", "M", "Homme", "H"),
  hf_name_pattern = "HF_name_visited",
  hf_start_pattern = "date_start_HF_visited",
  hf_end_pattern = "date_end_HF_visited",
  disease_col = "#f2b0b0",
  incubation_col = "#ffe6cc",
  onset_col = "darkred",
  recovered_col = "#add8e6",
  died_col = "#7f7f7f",
  recovered_value = "Recovered",
  default_n = 10
) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observeEvent(input$toggle_sidebar, {
      bslib::sidebar_toggle("timeline_sidebar")
    })

    df_mod <- shiny::reactive({
      df_out <- if (shiny::is.reactive(df)) df() else df

      required <- c(id_var, date_onset, date_exit, outcome_var, age_var, sex_var)
      missing <- setdiff(unique(required), names(df_out))
      if (length(missing)) {
        cli::cli_abort("{.arg df} is missing column{?s}: {.val {missing}}.")
      }
      df_out
    })

    # case-level data: one row per case, onset -> exit ---------------------
    cases_dat <- shiny::reactive({
      show_agesex <- isTRUE(input$show_agesex)
      df_mod() |>
        # cases without both dates or a final outcome cannot be drawn
        dplyr::filter(
          .data[[outcome_var]] %in% outcome_levels,
          !is.na(.data[[date_onset]]),
          !is.na(.data[[date_exit]])
        ) |>
        dplyr::transmute(
          .id = as.character(.data[[id_var]]),
          name = .id,
          age = .data[[age_var]],
          sex = .data[[sex_var]],
          outcome = as.character(.data[[outcome_var]]),
          date_onset = as.Date(.data[[date_onset]]),
          date_exit = as.Date(.data[[date_exit]])
        ) |>
        dplyr::mutate(
          sex_age = paste0(dplyr::if_else(sex %in% male_values, "M", "F"), age),
          label = if (show_agesex) paste0(name, " (", sex_age, ")") else name,
          onset_end = date_onset + 1,
          exit_end = date_exit + 1
        )
    })

    # visit-level data: one row per structure visited ----------------------
    visits_dat <- shiny::reactive({
      long <- df_mod() |>
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
          # initials, e.g. "CH La Guerison" -> "CLG"
          hf_abbr = purrr::map_chr(
            stringr::str_split(hf_name, "\\s+"),
            \(w) paste(stringr::str_to_upper(stringr::str_sub(w, 1, 1)), collapse = "")
          ),
          hf_start = as.Date(.data[[hf_start_pattern]]),
          hf_end_raw = as.Date(.data[[hf_end_pattern]])
        ) |>
        # a visit without a start date cannot be placed on the axis
        dplyr::filter(!is.na(hf_start))

      long |>
        dplyr::inner_join(
          dplyr::select(cases_dat(), .id, label, exit_end),
          by = dplyr::join_by(.id),
          relationship = "many-to-one"
        ) |>
        dplyr::mutate(
          is_last_visit = hf_start == max(hf_start),
          .by = .id
        ) |>
        # +1 so the last day fills its cell
        dplyr::mutate(
          hf_end = dplyr::case_when(
            !is.na(hf_end_raw) & hf_end_raw >= hf_start ~ hf_end_raw + 1,
            is_last_visit ~ exit_end,
            .default = hf_start + 1
          )
        )
    })

    shiny::observe({
      fac_choices <- sort(unique(visits_dat()$hf_name))
      shinyWidgets::updatePickerInput(
        session,
        "facility",
        choices = c("All facilities" = "", fac_choices),
        selected = shiny::isolate(input$facility)
      )
    })

    facility_ids <- shiny::reactive({
      ids <- cases_dat()$.id
      fac <- input$facility
      if (!is.null(fac) && length(fac) == 1 && nzchar(fac)) {
        ids <- intersect(ids, visits_dat()$.id[visits_dat()$hf_name == fac])
      }
      ids
    })

    # a facility change resets the selection to the latest default_n by onset
    prev_fac <- shiny::reactiveVal(NULL)
    shiny::observe({
      pool <- cases_dat() |>
        dplyr::filter(.id %in% facility_ids()) |>
        dplyr::arrange(date_onset)
      choices <- stats::setNames(pool$.id, pool$label)

      fac <- input$facility
      facility_changed <- !identical(fac, shiny::isolate(prev_fac()))
      prev_fac(fac)

      latest <- utils::tail(pool$.id, default_n)
      if (facility_changed) {
        sel <- latest
      } else {
        sel <- intersect(shiny::isolate(input$cases), pool$.id)
        if (!length(sel)) {
          sel <- latest
        }
      }

      shinyWidgets::updatePickerInput(
        session,
        "cases",
        choices = choices,
        selected = sel
      )
    })

    plot_ids <- shiny::reactive({
      sel <- input$cases
      if (is.null(sel) || !length(sel)) {
        return(character(0))
      }
      intersect(facility_ids(), sel)
    })

    # fixed row height, so many cases scroll instead of squashing the rows
    row_px <- 40
    output$chart_ui <- shiny::renderUI({
      n <- length(plot_ids())
      height <- if (n > 0) max(n * row_px + 80, 240) else 240
      highcharter::highchartOutput(ns("chart"), height = paste0(height, "px"))
    })

    output$chart <- highcharter::renderHighchart({
      ids <- plot_ids()
      shiny::validate(shiny::need(length(ids) > 0, "No cases to display"))

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

      incub_days <- input$incubation
      if (is.null(incub_days) || is.na(incub_days)) {
        incub_days <- 0
      }
      tl <- dplyr::mutate(tl, incub_start = date_onset - incub_days)

      # gridlines at midnights, day labels centred at noon
      range_start <- min(c(tl$incub_start, tl$date_onset, hf$hf_start))
      range_end <- max(c(tl$exit_end, hf$hf_end))
      midnights <- seq(range_start, range_end, by = "day")
      grid_lines <- lapply(
        to_ms(midnights),
        \(v) list(value = v, color = "#d9d9d9", width = 1, zIndex = 1)
      )
      day_labels <- to_ms(utils::head(midnights, -1)) + day_ms / 2

      # one month label centred over each month's visible span
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

      fmt <- function(d) format(d, "%d %b %Y")

      bar_pts <- tl |>
        dplyr::transmute(
          y,
          x = to_ms(date_onset),
          x2 = to_ms(exit_end),
          color = disease_col,
          outcome,
          d_start = fmt(date_onset),
          d_end = fmt(date_exit)
        )

      onset_pts <- tl |>
        dplyr::transmute(
          y,
          x = to_ms(date_onset),
          x2 = to_ms(onset_end),
          color = onset_col,
          d_start = fmt(date_onset)
        )

      # empty when the slider is at 0
      incub_pts <- tl |>
        dplyr::filter(incub_days > 0) |>
        dplyr::transmute(
          y,
          x = to_ms(incub_start),
          x2 = to_ms(date_onset),
          color = incubation_col,
          d_start = fmt(incub_start),
          d_end = fmt(date_onset)
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
          d_end = fmt(date_exit)
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
          d_start = fmt(hf_start),
          d_end = fmt(hf_end - 1)
        )

      month_fmt <- highcharter::JS(
        "function() { var m = ['January','February','March','April','May','June','July','August','September','October','November','December']; var d = new Date(this.value); return m[d.getUTCMonth()] + ' ' + d.getUTCFullYear(); }"
      )

      highcharter::highchart() |>
        highcharter::hc_chart(type = "xrange", animation = FALSE) |>
        highcharter::hc_title(text = NULL) |>
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
          name = "Disease",
          data = highcharter::list_parse(bar_pts),
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Disease ({point.outcome})</b><br>Onset: {point.d_start}<br>Exit: {point.d_end}"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Assumed incubation",
          data = highcharter::list_parse(incub_pts),
          enableMouseTracking = nrow(incub_pts) > 0,
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Assumed incubation</b><br>From {point.d_start} to {point.d_end}"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Facilities",
          data = highcharter::list_parse(hf_pts),
          dataLabels = list(enabled = TRUE, format = "{point.abbr}"),
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>{point.hf_name}</b><br>From {point.d_start} to {point.d_end}"
          )
        ) |>
        # onset and exit last so they stay above the structure rectangles
        highcharter::hc_add_series(
          name = "Symptom onset",
          data = highcharter::list_parse(onset_pts),
          showInLegend = FALSE,
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Symptom onset</b><br>{point.d_start}"
          )
        ) |>
        highcharter::hc_add_series(
          name = "Exit",
          data = highcharter::list_parse(exit_pts),
          showInLegend = FALSE,
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Exit ({point.outcome})</b><br>{point.d_end}"
          )
        ) |>
        highcharter::hc_legend(enabled = FALSE)
    })

    shiny::reactive(plot_ids())
  })
}
