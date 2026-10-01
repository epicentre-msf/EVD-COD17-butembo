# Lab data tab: value boxes (samples with whole-blood share, positives with
# positivity, mean delay from sampling to result) above the samples-tested curve.
# Uses the vb_* helpers from mod_vb.R and the lab_* constants from global.R.

mod_lab_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = tags$span(bsicons::bs_icon("clipboard-data"), "Lab data"),
    value = "lab",
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
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

mod_lab_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    time_server(
      id = "curve",
      df = df,
      date_vars = lab_date_vars,
      group_vars = lab_group_vars,
      group_pal = lab_pal,
      show_ratio = TRUE,
      ratio_var = "lab_result",
      ratio_lab = "Positivity",
      ratio_numer = "Positif",
      ratio_denom = c("Positif", "Négatif")
    )

    # ratio line is opt-in in epishiny, but it is the main read of this tab
    observe({
      updateCheckboxInput(session, "curve-show_ratio_line", value = TRUE)
    }) |>
      bindEvent(TRUE, once = TRUE)

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

      list(
        n_all = nrow(d),
        n_blood = sum(d$sample_type == "Sang total", na.rm = TRUE),
        n_pos = n_pos,
        n_tested = n_tested,
        delay_mean = if (length(delay)) mean(delay) else NA_real_,
        n_delay = length(delay)
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

    output$delay_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(label = "With both dates", value = vb_pct(s$n_delay, s$n_all))
      ))
    })
  })
}
