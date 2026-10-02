# Lab data tab: value boxes (samples with whole-blood share, positives with
# positivity, mean delay from sampling to result) above the samples-tested curve.
# Uses the vb_* helpers from mod_vb.R and the lab_* constants from global.R.

mod_lab_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = tags$span(bsicons::bs_icon("clipboard-data"), "Lab data"),
    value = "lab",
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      fill = FALSE,
      class = "pt-2 hgap-tight",
      value_box(
        title = "Samples",
        value = textOutput(ns("samples"), inline = TRUE),
        p(uiOutput(ns("samples_info"), inline = TRUE)),
        showcase_layout = "left center",
        class = "vb-accent vb-lab-samples",
        height = "85px"
      ),
      value_box(
        title = "Positive",
        value = textOutput(ns("positive"), inline = TRUE),
        p(uiOutput(ns("positive_info"), inline = TRUE)),
        showcase_layout = "left center",
        class = "vb-accent vb-lab-positive",
        height = "85px"
      ),
      value_box(
        title = vb_title_info(
          "Time from sampling to result",
          paste0(
            "Mean days from sampling to lab result.<br><br>",
            "Samples with a missing date or a result before sampling are excluded."
          )
        ),
        value = textOutput(ns("delay"), inline = TRUE),
        p(uiOutput(ns("delay_info"), inline = TRUE)),
        showcase_layout = "left center",
        class = "vb-accent vb-lab-delay",
        height = "85px"
      ),
      value_box(
        title = "Latest lab result",
        value = textOutput(ns("latest"), inline = TRUE),
        p(uiOutput(ns("latest_info"), inline = TRUE)),
        showcase_layout = "left center",
        class = "vb-accent vb-lab-latest",
        height = "85px"
      )
    ),
    time_ui(
      id = ns("curve"),
      title = "Samples tested",
      date_vars = lab_date_vars,
      group_vars = lab_group_vars,
      date_interval_default = "week",
      group_var_default = "lab_result",
      ratio_line_lab = "Show positivity line?"
    )
  )
}

# Replaces epishiny::time_server, whose positivity line is pooled across groups.
# Reuses time_ui's input ids, so the UI is unchanged.
lab_curve_server <- function(id, df, date_vars, group_vars, group_pal) {
  moduleServer(id, function(input, output, session) {
    na_label <- getOption("epishiny.na.label", "(Missing)")
    tested_levels <- c("Positif", "Négatif")

    observeEvent(input$toggle_sidebar, {
      bslib::sidebar_toggle("time_sidebar")
    })

    # time_ui always renders these, but the lab curve has no use for them
    shinyjs::hide("date")
    shinyjs::hide("count_var")
    shinyjs::hide("add_zoom_control")

    observe({
      cond <- input$group != "n"
      if (!cond) {
        shinyWidgets::updateRadioGroupButtons(
          session,
          "bar_stacking",
          selected = "normal"
        )
      }
      shinyjs::toggle("bar_stacking", condition = cond, anim = TRUE)
    })

    df_curve <- reactive({
      d <- if (is.reactive(df)) df() else df
      date_var <- input$date
      grp <- input$group
      interval <- tolower(input$date_interval)
      req(date_var, grp, interval)

      n_no_date <- sum(is.na(d[[date_var]]))
      message(
        "Lab curve: dropped ",
        n_no_date,
        " samples with a missing date; ",
        sum(!d$lab_result %in% tested_levels),
        " of ",
        nrow(d),
        " are not Positif/Négatif and stay out of positivity"
      )

      d <- d |>
        dplyr::mutate(
          period = lubridate::floor_date(
            lubridate::as_date(.data[[date_var]]),
            unit = interval,
            week_start = getOption("epishiny.week.start", 1)
          ),
          grp = if (grp == "n") "All samples" else .data[[grp]],
          is_tested = .data$lab_result %in% tested_levels,
          is_pos = .data$lab_result %in% "Positif"
        ) |>
        dplyr::filter(!is.na(.data$period))

      # keep missing groups visible rather than dropping them
      d$grp <- if (is.factor(d$grp)) {
        forcats::fct_na_value_to_level(d$grp, level = na_label)
      } else {
        factor(tidyr::replace_na(as.character(d$grp), na_label))
      }

      out <- d |>
        dplyr::summarise(
          .by = c("period", "grp"),
          n = dplyr::n(),
          n_tested = sum(.data$is_tested),
          n_pos = sum(.data$is_pos)
        )

      if (nrow(out) > 0) {
        out <- out |>
          tidyr::complete(
            period = seq.Date(
              min(.data$period),
              max(.data$period),
              by = interval
            ),
            tidyr::nesting(grp),
            fill = list(n = 0L, n_tested = 0L, n_pos = 0L)
          ) |>
          dplyr::arrange(.data$period) |>
          dplyr::mutate(
            .by = "grp",
            n_c = cumsum(.data$n),
            ratio = dplyr::if_else(
              .data$n_tested > 0,
              .data$n_pos / .data$n_tested * 100,
              NA_real_
            ),
            ratio_c = dplyr::if_else(
              cumsum(.data$n_tested) > 0,
              cumsum(.data$n_pos) / cumsum(.data$n_tested) * 100,
              NA_real_
            ),
            x = highcharter::datetime_to_timestamp(.data$period)
          )

        # per-result positivity is 0% / 100%, so use positives / tested overall
        if (grp == "lab_result") {
          pooled <- out |>
            dplyr::summarise(
              .by = "period",
              n_tested = sum(.data$n_tested),
              n_pos = sum(.data$n_pos)
            ) |>
            dplyr::arrange(.data$period) |>
            dplyr::mutate(
              ratio = dplyr::if_else(
                .data$n_tested > 0,
                .data$n_pos / .data$n_tested * 100,
                NA_real_
              ),
              ratio_c = dplyr::if_else(
                cumsum(.data$n_tested) > 0,
                cumsum(.data$n_pos) / cumsum(.data$n_tested) * 100,
                NA_real_
              )
            ) |>
            dplyr::select("period", "ratio", "ratio_c")

          n_before <- nrow(out)
          out <- out |>
            dplyr::select(-"ratio", -"ratio_c") |>
            dplyr::left_join(
              pooled,
              by = dplyr::join_by(period),
              relationship = "many-to-one"
            )
          stopifnot(nrow(out) == n_before)
        }
      }
      attr(out, "n_no_date") <- n_no_date
      out
    })

    output$chart <- highcharter::renderHighchart({
      out <- df_curve()
      shiny::validate(shiny::need(nrow(out) > 0, "No data to display"))

      grp <- input$group
      date_var <- input$date
      cumul <- isTruthy(input$cumulative)
      show_line <- isTruthy(input$show_ratio_line)
      n_no_date <- attr(out, "n_no_date")

      out$y_n <- if (cumul) out$n_c else out$n
      out$y_r <- if (cumul) out$ratio_c else out$ratio

      groups <- levels(out$grp)
      # one pooled line when groups are results, or when there are no groups
      pooled_line <- grp %in% c("n", "lab_result")
      pal <- if (grp == "n") {
        "#9aa5b1"
      } else {
        epishiny:::prepare_palette(
          length(groups),
          na_label %in% groups,
          pal = if (grp == "lab_result") {
            group_pal
          } else {
            epishiny:::epi_pals()$frost
          },
          na_colour = "#666666"
        )
      }
      # line sits on same-coloured bars, so darken it to stay readable
      line_pal <- if (grp == "n") {
        "black"
      } else {
        vapply(
          pal,
          \(col) grDevices::colorRampPalette(c(col, "black"))(5)[3],
          character(1)
        )
      }

      hc <- highcharter::highchart() |>
        highcharter::hc_chart(zoomType = "x")

      for (i in seq_along(groups)) {
        d_g <- out[out$grp == groups[i], ]
        hc <- hc |>
          highcharter::hc_add_series(
            data = d_g,
            type = "column",
            highcharter::hcaes(x = x, y = y_n),
            name = if (grp == "n") "Samples" else groups[i],
            id = paste0("col_", i),
            color = pal[i]
          )
        # no positivity line for samples with an unknown group
        if (show_line && !pooled_line && groups[i] != na_label) {
          hc <- hc |>
            highcharter::hc_add_series(
              data = d_g,
              type = "line",
              highcharter::hcaes(x = x, y = y_r),
              name = paste("Positivity:", groups[i]),
              linkedTo = paste0("col_", i),
              yAxis = 1,
              zIndex = 10,
              color = line_pal[i],
              marker = list(enabled = TRUE, radius = 3),
              tooltip = list(valueDecimals = 1, valueSuffix = "%")
            )
        }
      }

      if (show_line && pooled_line) {
        hc <- hc |>
          highcharter::hc_add_series(
            data = out[out$grp == groups[1], ],
            type = "line",
            highcharter::hcaes(x = x, y = y_r),
            name = "Positivity",
            yAxis = 1,
            zIndex = 10,
            color = "black",
            marker = list(enabled = TRUE, radius = 3),
            tooltip = list(valueDecimals = 1, valueSuffix = "%")
          )
      }

      y_axes <- list(
        list(
          title = list(
            text = if (cumul) "Cumulative number of samples" else "Number of samples"
          ),
          allowDecimals = FALSE
        )
      )
      if (show_line) {
        y_axes <- c(
          y_axes,
          list(list(
            title = list(text = if (cumul) "Cumulative positivity" else "Positivity"),
            labels = list(enabled = TRUE, format = "{value}%"),
            min = 0,
            max = 100,
            # explicit ticks stop Highcharts rounding the axis up to 120
            tickPositions = list(0, 20, 40, 60, 80, 100),
            endOnTick = FALSE,
            gridLineWidth = 0,
            opposite = TRUE
          ))
        )
      }

      date_lab <- names(date_vars)[date_vars == date_var]
      group_lab <- names(group_vars)[group_vars == grp]

      hc <- hc |>
        highcharter::hc_title(text = NULL) |>
        highcharter::hc_xAxis(
          type = "datetime",
          title = list(text = date_lab),
          lineColor = "black",
          crosshair = TRUE
        ) |>
        (\(h) do.call(highcharter::hc_yAxis_multiples, c(list(h), y_axes)))() |>
        highcharter::hc_plotOptions(
          column = list(
            stacking = if (grp == "n") "normal" else isolate(input$bar_stacking)
          )
        ) |>
        highcharter::hc_tooltip(shared = TRUE) |>
        highcharter::hc_legend(
          enabled = grp != "n",
          title = list(text = group_lab),
          layout = "vertical",
          align = "right",
          verticalAlign = "top",
          x = -10,
          y = 40,
          itemStyle = list(textOverflow = "ellipsis", width = 150)
        ) |>
        highcharter::hc_exporting(enabled = TRUE)

      if (isolate(tolower(input$date_interval)) == "week") {
        hc <- hc |>
          highcharter::hc_xAxis(
            labels = list(formatter = epishiny:::hc_week_labels())
          )
      }

      if (n_no_date > 0) {
        hc <- hc |>
          highcharter::hc_credits(
            enabled = TRUE,
            text = paste0(
              "Missing ",
              tolower(date_lab),
              " for ",
              scales::number(n_no_date),
              " samples"
            )
          )
      }
      hc
    })

    observe({
      highcharter::highchartProxy(session$ns("chart")) |>
        highcharter::hcpxy_update(
          plotOptions = list(column = list(stacking = input$bar_stacking))
        )
    }) |>
      bindEvent(input$bar_stacking, ignoreInit = TRUE)
  })
}

mod_lab_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    lab_curve_server(
      id = "curve",
      df = df,
      date_vars = lab_date_vars,
      group_vars = lab_group_vars,
      group_pal = lab_pal
    )

    # time_ui hardcodes the switch to FALSE, so flip it once at start
    observeEvent(
      TRUE,
      {
        bslib::update_switch("curve-show_ratio_line", value = TRUE)
      },
      once = TRUE
    )

    df_summary <- reactive({
      d <- if (is.reactive(df)) df() else df

      # positivity denominator is tested samples only, as in the chart
      n_pos <- sum(d$lab_result == "Positif", na.rm = TRUE)
      n_tested <- sum(d$lab_result %in% c("Positif", "Négatif"))

      delay <- as.numeric(d$date_lab_result - d$date_sampling)
      message(
        "Lab delay: dropped ",
        sum(is.na(delay)),
        " with a missing date and ",
        sum(delay < 0, na.rm = TRUE),
        " with a negative interval, of ",
        nrow(d),
        " samples"
      )
      delay <- delay[!is.na(delay) & delay >= 0]

      latest_date <- function(x) {
        if (all(is.na(x))) as.Date(NA) else max(x, na.rm = TRUE)
      }
      latest <- latest_date(d$date_lab_result)
      latest_by_source <- function(src) {
        latest_date(d$date_lab_result[d$source %in% src])
      }

      list(
        n_all = nrow(d),
        n_blood = sum(d$sample_type == "Sang total", na.rm = TRUE),
        n_pos = n_pos,
        n_tested = n_tested,
        delay_mean = if (length(delay)) mean(delay) else NA_real_,
        n_delay = length(delay),
        latest = latest,
        latest_mobile = latest_by_source("Laboratoire Mobile"),
        latest_inrb = latest_by_source("INRB Béni")
      )
    })

    output$samples <- renderText(scales::number(df_summary()$n_all))

    output$samples_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(
          label = "Whole blood",
          value = paste0(
            scales::number(s$n_blood),
            " (",
            vb_pct(s$n_blood, s$n_all),
            ")"
          )
        )
      ))
    })

    output$positive <- renderText(scales::number(df_summary()$n_pos))

    output$positive_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(label = "Positivity", value = vb_pct(s$n_pos, s$n_tested))
      ))
    })

    output$delay <- renderText({
      m <- df_summary()$delay_mean
      if (is.na(m)) "—" else paste(scales::number(m, accuracy = 0.1), "days")
    })

    output$latest <- renderText({
      s <- df_summary()
      if (is.na(s$latest)) "—" else format(s$latest, "%d %b %Y")
    })

    output$latest_info <- renderUI({
      s <- df_summary()
      fmt <- \(x) if (is.na(x)) "—" else format(x, "%d %b")
      vb_stat_line(list(
        list(label = "Mobile lab", value = fmt(s$latest_mobile)),
        list(label = "INRB", value = fmt(s$latest_inrb))
      ))
    })

    output$delay_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(label = "With both dates", value = vb_pct(s$n_delay, s$n_all))
      ))
    })
  })
}
