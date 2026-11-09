# Care-pathway timeline: one row per case, onset to exit, structures visited
# drawn over the illness bar. Local copy of the epishiny.timeline module, so the
# dashboard carries no dependency on that package.
# Cases are labelled by id only (plus sex / age), as no patient names reach the app.

mod_timeline_ui <- function(
  id,
  title = "Parcours de soins",
  icon = bsicons::bs_icon("activity"),
  facility_lab = "Formation sanitaire",
  cases_lab = "Cas à afficher",
  agesex_lab = "Afficher âge / sexe",
  incubation_lab = "Incubation supposée (jours)",
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

  # lives above the chart, not in the options sidebar: these are the main filters
  filters_ui <- htmltools::tags$div(
    class = "tl-filters d-flex flex-wrap align-items-end gap-3",
    htmltools::tags$style(
      ".tl-filters .form-group { margin-bottom: 0; }
       .datepicker-dropdown { z-index: 2000 !important; }"
    ),
    shinyWidgets::pickerInput(
      ns("facility"),
      facility_lab,
      choices = character(0),
      multiple = FALSE,
      width = "280px",
      options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        # body, so the card's overflow cannot clip the list
        container = "body",
        noneSelectedText = "Toutes les formations sanitaires"
      )
    ),
    shiny::dateRangeInput(
      ns("daterange"),
      "Période (cas présents sur ces dates)",
      language = "fr",
      weekstart = 1,
      format = "dd/mm/yyyy",
      separator = " au ",
      width = "300px"
    ),
    shinyWidgets::radioGroupButtons(
      ns("preset"),
      "Raccourcis",
      choices = c(
        "7 j" = "7",
        "14 j" = "14",
        "30 j" = "30",
        "Tout" = "all"
      ),
      selected = "30",
      size = "sm"
    )
  )

  inputs_ui <- shiny::tagList(
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
      fill = FALSE,
      class = "pb-0",
      filters_ui
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
  female_values = c("Female", "F", "Femme"),
  hf_name_pattern = "HF_name_visited",
  hf_start_pattern = "date_start_HF_visited",
  hf_end_pattern = "date_end_HF_visited",
  hf_type_pattern = "HF_type_visited",
  disease_col = "#f2b0b0",
  pending_col = "#7b4fa3",
  isolation_col = "#3a9d8f",
  incubation_col = "#ffe6cc",
  onset_col = "darkred",
  recovered_col = group_pals$type_of_exit[["Recovered"]],
  died_col = group_pals$type_of_exit[["Died"]],
  recovered_value = "Recovered",
  default_n = 10,
  default_facility = "wanamahika"
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
        # no onset, no bar. Cases without a settled exit stay, drawn up to
        # their last known day in care
        dplyr::filter(!is.na(.data[[date_onset]])) |>
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
          # settled = a final outcome and a date for it
          resolved = outcome %in% outcome_levels & !is.na(date_exit),
          date_exit = dplyr::if_else(resolved, date_exit, as.Date(NA)),
          # anything else, including NA, is unknown: not a default female
          sex_letter = dplyr::case_when(
            sex %in% male_values ~ "M",
            sex %in% female_values ~ "F",
            .default = "?"
          ),
          sex_age = paste0(sex_letter, dplyr::coalesce(as.character(age), "?")),
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
          dplyr::contains(hf_end_pattern),
          dplyr::contains(hf_type_pattern)
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
          hf_end_raw = as.Date(.data[[hf_end_pattern]]),
          is_isolation = .data[[hf_type_pattern]] %in% "isolation"
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
            is_last_visit & !is.na(exit_end) ~ exit_end,
            .default = hf_start + 1
          )
        )
    })

    # first and last day each case is on record, from onset, exit and visits
    case_span <- shiny::reactive({
      v <- visits_dat() |>
        dplyr::summarise(
          .by = .id,
          v_start = min(hf_start),
          v_end = max(hf_end)
        )
      cases_dat() |>
        dplyr::left_join(
          v,
          by = dplyr::join_by(.id),
          relationship = "one-to-one"
        ) |>
        dplyr::mutate(
          span_start = pmin(date_onset, v_start, na.rm = TRUE),
          # exclusive ends, so the last day on record is span_end - 1
          span_last = pmax(exit_end, v_end, onset_end, na.rm = TRUE) - 1
        ) |>
        dplyr::select(.id, span_start, span_last)
    })

    extent <- shiny::reactive({
      sp <- case_span()
      shiny::req(nrow(sp) > 0)
      c(min(sp$span_start), max(sp$span_last))
    })

    # presets count back from the latest date on record, not from today
    shiny::observeEvent(list(input$preset, extent()), {
      e <- extent()
      from <- if (identical(input$preset, "all")) {
        e[[1]]
      } else {
        max(e[[1]], e[[2]] - as.integer(input$preset) + 1L)
      }
      shiny::updateDateRangeInput(
        session,
        "daterange",
        start = from,
        end = e[[2]],
        min = e[[1]],
        max = e[[2]]
      )
    })

    # a case counts when its stay overlaps the range at all
    range_ids <- shiny::reactive({
      sp <- case_span()
      r <- input$daterange
      if (is.null(r) || anyNA(r)) {
        return(sp$.id)
      }
      sp$.id[sp$span_start <= r[[2]] & sp$span_last >= r[[1]]]
    })

    date_filter_active <- shiny::reactive({
      r <- input$daterange
      !is.null(r) && !anyNA(r) && !identical(unname(r), unname(extent()))
    })

    # the default structure is picked once, on the first non-empty choice list
    default_set <- shiny::reactiveVal(FALSE)
    shiny::observe({
      fac_choices <- sort(unique(visits_dat()$hf_name))
      sel <- shiny::isolate(input$facility)
      if (!shiny::isolate(default_set()) && length(fac_choices)) {
        hit <- grep(default_facility, fac_choices, ignore.case = TRUE, value = TRUE)
        if (length(hit)) {
          sel <- hit[[1]]
        }
        default_set(TRUE)
      }
      shinyWidgets::updatePickerInput(
        session,
        "facility",
        choices = c("Toutes les formations sanitaires" = "", fac_choices),
        selected = sel
      )
    })

    facility_ids <- shiny::reactive({
      ids <- intersect(cases_dat()$.id, range_ids())
      fac <- input$facility
      if (!is.null(fac) && length(fac) == 1 && nzchar(fac)) {
        ids <- intersect(ids, visits_dat()$.id[visits_dat()$hf_name == fac])
      }
      ids
    })

    # a facility or period change resets the selection: the latest default_n by
    # onset, or every case in range once a period is set
    prev_fac <- shiny::reactiveVal(NULL)
    shiny::observe({
      pool <- cases_dat() |>
        dplyr::filter(.id %in% facility_ids()) |>
        dplyr::arrange(date_onset)
      choices <- stats::setNames(pool$.id, pool$label)

      fac <- list(input$facility, input$daterange)
      facility_changed <- !identical(fac, shiny::isolate(prev_fac()))
      prev_fac(fac)

      latest <- if (date_filter_active()) {
        pool$.id
      } else {
        utils::tail(pool$.id, default_n)
      }
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

      incub_days <- input$incubation
      if (is.null(incub_days) || is.na(incub_days)) {
        incub_days <- 0
      }
      # unsettled cases end on their last known day in care, whatever the
      # structure recorded, so the purple marker sits on a real date
      last_known <- hf |>
        dplyr::summarise(.by = .id, last_end = max(hf_end))
      tl <- tl |>
        dplyr::left_join(
          last_known,
          by = dplyr::join_by(.id),
          relationship = "one-to-one"
        ) |>
        dplyr::mutate(
          exit_end = dplyr::if_else(
            resolved,
            exit_end,
            pmax(last_end, onset_end, na.rm = TRUE)
          ),
          incub_start = date_onset - incub_days
        )

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
          outcome = dplyr::if_else(resolved, outcome, "issue inconnue"),
          end_lab = dplyr::if_else(
            resolved,
            "Sortie",
            "Dernière hospitalisation connue"
          ),
          d_start = fmt(date_onset),
          d_end = fmt(exit_end - 1)
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

      pending_pts <- tl |>
        dplyr::filter(!resolved) |>
        dplyr::transmute(
          y,
          x = to_ms(exit_end - 1),
          x2 = to_ms(exit_end),
          color = pending_col,
          d_end = fmt(exit_end - 1)
        )

      exit_pts <- tl |>
        dplyr::filter(resolved) |>
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
          # isolation stays are filled, other visits stay outlined
          color = dplyr::if_else(is_isolation, isolation_col, "rgba(0,0,0,0)"),
          borderColor = dplyr::if_else(is_isolation, "#1f6f64", "#262626"),
          hf_name,
          hf_kind = dplyr::if_else(is_isolation, "Isolement", "Visite"),
          abbr = hf_abbr,
          d_start = fmt(hf_start),
          d_end = fmt(hf_end - 1)
        )

      month_fmt <- highcharter::JS(
        "function() { var m = ['Janvier','Février','Mars','Avril','Mai','Juin','Juillet','Août','Septembre','Octobre','Novembre','Décembre']; var d = new Date(this.value); return m[d.getUTCMonth()] + ' ' + d.getUTCFullYear(); }"
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
            tickPositions = as.list(day_labels),
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
            tickPositions = as.list(month_labels),
            labels = list(
              formatter = month_fmt,
              style = list(fontWeight = "bold", fontSize = "12px")
            )
          )
        ) |>
        highcharter::hc_yAxis(
          # as.list: a lone value would serialise as a string, not an array
          categories = as.list(cats),
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
            pointFormat = "<b>Maladie ({point.outcome})</b><br>Début : {point.d_start}<br>{point.end_lab} : {point.d_end}"
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
          name = "Formations sanitaires",
          data = highcharter::list_parse(hf_pts),
          dataLabels = list(enabled = TRUE, format = "{point.abbr}"),
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>{point.hf_name}</b> ({point.hf_kind})<br>Du {point.d_start} au {point.d_end}"
          )
        ) |>
        # onset, exit and pending last so they stay above the structure rectangles
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
          name = "Actif / issue inconnue",
          data = highcharter::list_parse(pending_pts),
          showInLegend = FALSE,
          enableMouseTracking = nrow(pending_pts) > 0,
          tooltip = list(
            headerFormat = "",
            pointFormat = "<b>Actif ou issue inconnue</b><br>Dernière hospitalisation connue : {point.d_end}"
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

    shiny::reactive(plot_ids())
  })
}
