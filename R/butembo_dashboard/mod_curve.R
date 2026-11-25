# Main epicurve: epishiny::time_ui / time_server, with the fixed ratio line
# (set once at start-up) replaced by a proportion line the user picks: the
# share of one level of the current grouping among all cases with a known
# value, per time unit. Copied from epishiny 0.1.0 time_server; ids match
# time_ui, so recolour.js and the bar-click time filter keep working.

#' time_ui with a "Proportion de" level picker under the line switch
#' @param ... passed to epishiny::time_ui
curve_ui <- function(
  id,
  ...,
  ratio_level_lab = "Proportion de",
  ratio_smooth_lab = "Lissage (moyenne mobile)",
  ratio_valid_lab = "% valides uniquement (exclure les manquants)"
) {
  ns <- NS(id)
  ui <- time_ui(id = id, ...)
  level_select <- conditionalPanel(
    condition = "input.show_ratio_line",
    ns = ns,
    selectInput(
      ns("ratio_level"),
      label = ratio_level_lab,
      choices = NULL,
      selectize = FALSE,
      width = "100%"
    ),
    # in units of the chosen date interval (weeks by default)
    selectInput(
      ns("ratio_smooth"),
      label = ratio_smooth_lab,
      choices = c("Aucun" = 1, "2 périodes" = 2, "3 périodes" = 3, "4 périodes" = 4, "6 périodes" = 6, "8 périodes" = 8),
      selected = 1,
      selectize = FALSE,
      width = "100%"
    ),
    bslib::input_switch(ns("ratio_valid"), ratio_valid_lab, value = TRUE)
  )
  htmltools::tagQuery(ui)$
    find(paste0("#", ns("show_ratio_line")))$
    parent()$
    parent()$
    after(level_select)$
    allTags()
}

#' @param group_pal fallback palette; recolour.js applies the per-level colours
#' @return reactive time filter from a bar click, as epishiny::time_server
curve_server <- function(
  id,
  df,
  date_vars,
  group_vars,
  group_pal,
  na_colour = "#666666",
  ratio_axis_lab = "Proportion (%)",
  place_filter = shiny::reactiveVal(),
  filter_info = shiny::reactiveVal(),
  filter_reset = shiny::reactiveVal()
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    na_label <- getOption("epishiny.na.label", "(Missing)")

    observeEvent(input$toggle_sidebar, {
      bslib::sidebar_toggle("time_sidebar")
    })

    # time_ui always renders these, but this curve has one count and no picker
    shinyjs::hide("count_var")
    if (length(date_vars) < 2) {
      shinyjs::hide("date")
    }

    # the line needs a grouping; the switch sits in a container with no id
    switch_box <- paste0(
      ".shiny-input-container:has(#",
      ns("show_ratio_line"),
      ")"
    )
    observe({
      grouped <- input$group != "n"
      if (!grouped) {
        shinyWidgets::updateRadioGroupButtons(
          session,
          "bar_stacking",
          selected = "normal"
        )
        bslib::update_switch("show_ratio_line", value = FALSE)
      }
      shinyjs::toggle("bar_stacking", condition = grouped, anim = TRUE)
      shinyjs::toggle(selector = switch_box, condition = grouped, anim = TRUE)
    })

    df_mod <- reactive({
      d <- epishiny:::force_reactive(df)
      pf <- place_filter()
      if (length(pf)) {
        d <- dplyr::filter(d, .data[[pf$geo_col]] == pf$region_select)
      }
      d
    })

    filter_info_out <- reactive({
      epishiny:::format_filter_info(filter_info(), pf = place_filter())
    })

    df_interval <- reactive({
      req(input$date, input$date_interval)
      dplyr::mutate(
        df_mod(),
        !!input$date := lubridate::floor_date(
          lubridate::as_date(.data[[input$date]]),
          unit = tolower(input$date_interval),
          week_start = getOption("epishiny.week.start", 1)
        )
      )
    })

    df_curve <- reactive({
      grp <- input$group
      epishiny:::get_time_df(
        df = df_interval(),
        is_agg = FALSE,
        is_grouped = grp != "n",
        date_var = input$date,
        count_var = NULL,
        group_var = grp,
        date_interval = tolower(input$date_interval)
      )
    })

    # levels of the current grouping that are present, missing left out
    ratio_choices <- reactive({
      grp <- input$group
      if (identical(grp, "n")) {
        return(character())
      }
      x <- df_mod()[[grp]]
      lv <- if (is.factor(x)) levels(droplevels(x)) else sort(unique(x))
      as.character(lv[!is.na(lv)])
    })

    observe({
      ch <- ratio_choices()
      sel <- if (isTRUE(input$ratio_level %in% ch)) input$ratio_level else ch[1]
      updateSelectInput(session, "ratio_level", choices = ch, selected = sel)
    })

    # valid % drops missing and "Not applicable"-type levels (na_labels, from
    # global.R) from the denominator; off, every case with a date counts
    # NULL rather than req(): the bars must still render while the picker
    # catches up with a new grouping. Smoothing pools the last k periods
    # (sum of n1 / sum of N), so a 2-case week weighs less than a 50-case one;
    # the window trails, so the latest point uses no future data
    df_ratio <- reactive({
      grp <- input$group
      lvl <- input$ratio_level
      if (!isTRUE(input$show_ratio_line) || !isTRUE(lvl %in% ratio_choices())) {
        return(NULL)
      }
      date_var <- input$date
      k <- as.integer(input$ratio_smooth %||% 1)
      roll <- \(x) {
        cs <- cumsum(x)
        cs - dplyr::lag(cs, k, default = 0)
      }
      valid_only <- isTRUE(input$ratio_valid %||% TRUE)
      d <- df_interval() |>
        dplyr::filter(
          !is.na(.data[[date_var]]),
          !valid_only |
            !(is.na(.data[[grp]]) | as.character(.data[[grp]]) %in% na_labels)
        ) |>
        dplyr::summarise(
          .by = dplyr::all_of(date_var),
          n1 = sum(as.character(.data[[grp]]) %in% lvl),
          N = dplyr::n()
        )
      if (nrow(d) == 0) {
        return(NULL)
      }
      # periods with no case must count in the window, so fill the gaps
      all_dates <- seq(
        min(d[[date_var]]),
        max(d[[date_var]]),
        by = tolower(input$date_interval)
      )
      d |>
        tidyr::complete(
          !!rlang::sym(date_var) := all_dates,
          fill = list(n1 = 0L, N = 0L)
        ) |>
        dplyr::arrange(.data[[date_var]]) |>
        dplyr::mutate(
          n1_k = roll(n1),
          N_k = roll(N),
          ratio = dplyr::if_else(N_k > 0, 100 * n1_k / N_k, NA_real_),
          ratio_c = 100 * cumsum(n1) / cumsum(N)
        )
    })

    output$chart <- highcharter::renderHighchart({
      df <- df_curve()
      dr <- df_ratio()
      date <- input$date
      date_sym <- rlang::sym(date)
      group <- input$group
      cumulative <- isTRUE(input$cumulative)
      n_var <- if (cumulative) "n_c" else "n"
      y_lab <- getOption("epishiny.count.label", "Cases")
      date_lab <- epishiny:::get_label(date, date_vars)
      shiny::validate(shiny::need(nrow(df) > 0, "Aucune donnée à afficher"))

      if (group == "n") {
        hc <- highcharter::hchart(
          df,
          "column",
          highcharter::hcaes(!!date_sym, !!rlang::sym(n_var)),
          id = "n_bars",
          name = y_lab
        )
      } else {
        group_lab <- epishiny:::get_label(group, group_vars)
        if (anyNA(df[[group]])) {
          df[[group]] <- forcats::fct_na_value_to_level(
            df[[group]],
            level = na_label
          )
        }
        pal <- epishiny:::prepare_palette(
          dplyr::n_distinct(df[[group]]),
          na_label %in% df[[group]],
          pal = group_pal,
          na_colour = na_colour
        )
        hc <- highcharter::hchart(
          df,
          "column",
          showInNavigator = TRUE,
          highcharter::hcaes(!!date_sym, !!rlang::sym(n_var), group = !!rlang::sym(group))
        ) |>
          highcharter::hc_legend(
            title = list(
              text = glue::glue(
                "{group_lab}<br/><span style=\"font-size: 9px; color: #666; font-weight: normal\">(cliquer pour filtrer)</span>"
              )
            ),
            layout = "vertical",
            align = "right",
            verticalAlign = "top",
            x = -10,
            y = 40,
            itemStyle = list(textOverflow = "ellipsis", width = 150)
          ) |>
          highcharter::hc_colors(pal)
      }

      click_js <- highcharter::JS(glue::glue(
        "function(event) {
          Shiny.setInputValue(\"{{ns(\"chart_click\")}}\", {x: event.point.x, y: event.point.y}, {priority: \"event\"});
        }",
        .open = "{{",
        .close = "}}"
      ))

      tf <- isolate(time_filter())
      plot_bands <- if (length(tf)) {
        pb <- epishiny:::get_plot_band_limits(tf)
        list(list(
          color = "#D3D3D350",
          zIndex = 1,
          from = highcharter::datetime_to_timestamp(pb$from),
          to = highcharter::datetime_to_timestamp(pb$to)
        ))
      } else {
        list()
      }

      x_axis <- list(
        title = list(text = date_lab),
        lineColor = "black",
        allowDecimals = FALSE,
        crosshair = TRUE,
        ordinal = TRUE,
        plotBands = plot_bands
      )
      if (tolower(input$date_interval) == "week") {
        x_axis$labels <- list(formatter = epishiny:::hc_week_labels())
      }

      has_line <- !is.null(dr) && nrow(dr) > 0
      hc <- do.call(highcharter::hc_xAxis, c(list(hc), x_axis))
      hc <- hc |>
        highcharter::hc_title(text = NULL) |>
        highcharter::hc_chart(zoomType = "x", alignTicks = FALSE) |>
        highcharter::hc_plotOptions(
          column = list(stacking = isolate(input$bar_stacking)),
          series = list(
            cursor = "pointer",
            stickyTracking = TRUE,
            events = list(click = click_js)
          )
        ) |>
        highcharter::hc_yAxis_multiples(
          list(title = list(text = y_lab), allowDecimals = FALSE),
          list(
            title = list(text = if (has_line) ratio_axis_lab else ""),
            labels = list(enabled = has_line, format = "{value}%"),
            min = 0,
            max = 100,
            opposite = TRUE,
            gridLineWidth = 0
          )
        ) |>
        highcharter::hc_boost(enabled = TRUE) |>
        epishiny:::my_hc_export(caption = isolate(filter_info_out()))

      if (has_line) {
        r_var <- if (cumulative) "ratio_c" else "ratio"
        hc <- hc |>
          highcharter::hc_add_series(
            data = dr,
            "line",
            highcharter::hcaes(x = !!date_sym, y = !!rlang::sym(r_var)),
            id = "ratio_line",
            name = paste0(
              "% ",
              input$ratio_level,
              if (!cumulative && as.integer(input$ratio_smooth %||% 1) > 1) {
                paste0(" (lissé sur ", input$ratio_smooth, " périodes)")
              }
            ),
            yAxis = 1,
            zIndex = 10,
            color = "black",
            marker = list(radius = 3),
            showInNavigator = FALSE
          )
      }

      missing_dates <- sum(is.na(df_mod()[[date]]))
      if (missing_dates > 0) {
        hc <- hc |>
          highcharter::hc_credits(
            enabled = TRUE,
            text = glue::glue(
              "{tolower(date_lab)} manquante pour {scales::number(missing_dates)} {tolower(y_lab)}"
            )
          )
      }
      hc
    }) |>
      bindEvent(
        df_curve(),
        df_ratio(),
        input$cumulative
      )

    observe({
      highcharter::highchartProxy(ns("chart")) |>
        highcharter::hcpxy_update(
          plotOptions = list(column = list(stacking = input$bar_stacking))
        )
    }) |>
      bindEvent(input$bar_stacking, ignoreInit = TRUE)

    # bar click -> time filter, unchanged from epishiny
    bar_click <- reactiveVal(NULL)
    observe({
      new_click <- input$chart_click$x
      bar_click(if (identical(bar_click(), new_click)) NULL else new_click)
    }) |>
      bindEvent(input$chart_click)
    observe(bar_click(NULL)) |> bindEvent(filter_reset(), ignoreInit = TRUE)
    observe(bar_click(NULL)) |>
      bindEvent(input$date_interval, ignoreInit = TRUE)

    time_filter <- reactive({
      bc <- bar_click()
      if (!length(bc)) {
        return(NULL)
      }
      interval <- tolower(input$date_interval)
      from <- lubridate::as_datetime(bc / 1000, origin = "1970-01-01")
      to <- from + lubridate::period(1, interval) - lubridate::period(1, "day")
      list(
        date_var = input$date,
        from = from,
        to = to,
        interval = interval,
        lab = epishiny:::format_period(c(from, to), interval)
      )
    })

    observe({
      tf <- time_filter()
      bands <- if (length(tf)) {
        pb <- epishiny:::get_plot_band_limits(tf)
        list(list(
          color = "#D3D3D350",
          zIndex = 1,
          from = highcharter::datetime_to_timestamp(pb$from),
          to = highcharter::datetime_to_timestamp(pb$to)
        ))
      } else {
        list()
      }
      highcharter::highchartProxy(ns("chart")) |>
        highcharter::hcpxy_update(xAxis = list(plotBands = bands))
    }) |>
      bindEvent(time_filter(), ignoreNULL = FALSE)

    reactive(time_filter())
  })
}
