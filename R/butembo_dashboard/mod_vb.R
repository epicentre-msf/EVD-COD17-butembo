# Value boxes above the dashboard: confirmed cases, notification status,
# MSF caseload, deaths, community deaths.

mod_vb_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(2, 2, 4, 2, 2),
    fill = FALSE,
    class = "pt-2 hgap-tight",
    value_box(
      title = vb_title_info(
        "Confirmed cases",
        "Denominator note: counts of confirmed cases only.",
        max_width = "260px"
      ),
      value = textOutput(ns("confirmed"), inline = TRUE),
      p(uiOutput(ns("confirmed_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-confirmed",
      height = "85px"
    ),
    value_box(
      title = vb_title_info(
        "Alive at notification",
        paste0(
          "Confirmed cases only.<br><br>",
          "Share alive when notified, from dead_upon_notif ",
          "(derived - dead_upon_arrival is dropped upstream). ",
          "Missing is the share with no recorded status."
        )
      ),
      value = textOutput(ns("alive_notif"), inline = TRUE),
      p(uiOutput(ns("alive_notif_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-alive",
      height = "85px"
    ),
    value_box(
      title = vb_title_info(
        "MSF confirmed patients",
        "Confirmed cases only. Cases with a non-missing id_msf."
      ),
      value = textOutput(ns("msf"), inline = TRUE),
      p(uiOutput(ns("msf_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-msf",
      height = "85px"
    ),
    value_box(
      title = vb_title_info(
        "Deaths",
        paste0(
          "Confirmed cases only.<br><br>",
          "CFR is deaths over cases with a known outcome ",
          "(Died or Recovered) - abandoned and unresolved exits excluded."
        )
      ),
      value = textOutput(ns("deaths"), inline = TRUE),
      p(uiOutput(ns("deaths_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-deaths",
      height = "85px"
    ),
    value_box(
      title = vb_title_info(
        "Non-isolated deaths",
        "Confirmed cases only. death_place is Community rather than CTE/CT."
      ),
      value = textOutput(ns("nonisolated_deaths"), inline = TRUE),
      p(uiOutput(ns("nonisolated_deaths_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-nonisolated",
      height = "85px"
    )
  )
}

mod_vb_server <- function(id, df, time_filter, place_filter) {
  moduleServer(id, function(input, output, session) {
    # the sidebar filter is applied upstream, but map and epicurve clicks are
    # not - reapply them here so the boxes track the charts
    df_summary <- reactive({
      d <- if (is.reactive(df)) df() else df
      pf <- place_filter()
      if (length(pf)) {
        d <- dplyr::filter(d, .data[[pf$geo_col]] == pf$region_select)
      }
      tf <- time_filter()
      if (length(tf)) {
        d <- dplyr::filter(
          d,
          dplyr::between(.data[[tf$date_var]], tf$from, tf$to)
        )
      }
      # every box below is confirmed cases only
      conf <- dplyr::filter(d, EVD_status == "Confirmed")
      n_confirmed <- nrow(conf)

      n_alive_notif <- sum(conf$dead_upon_notif == FALSE, na.rm = TRUE)
      n_missing_notif <- sum(is.na(conf$dead_upon_notif))

      n_msf <- sum(!is.na(conf$id_msf))

      n_died <- sum(conf$type_of_exit == "Died", na.rm = TRUE)
      n_recovered <- sum(conf$type_of_exit == "Recovered", na.rm = TRUE)

      # unclassified place of death counts as non-isolated, so the split sums to n_died
      n_nonisolated_deaths <- sum(
        conf$type_of_exit == "Died" & !conf$death_place %in% "CTE/CT",
        na.rm = TRUE
      )

      list(
        n_confirmed = n_confirmed,
        n_probable = sum(d$EVD_status == "Probable", na.rm = TRUE),
        n_alive_notif = n_alive_notif,
        n_missing_notif = n_missing_notif,
        n_msf = n_msf,
        n_died = n_died,
        n_recovered = n_recovered,
        n_nonisolated_deaths = n_nonisolated_deaths
      )
    })

    output$confirmed <- renderText(scales::number(df_summary()$n_confirmed))

    output$confirmed_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(label = "Probable", value = scales::number(s$n_probable))
      ))
    })

    output$alive_notif <- renderText({
      s <- df_summary()
      vb_pct(s$n_alive_notif, s$n_confirmed)
    })

    output$alive_notif_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(
          label = "Missing",
          value = vb_pct(s$n_missing_notif, s$n_confirmed)
        )
      ))
    })

    output$msf <- renderText(scales::number(df_summary()$n_msf))

    output$msf_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(
          label = "Of confirmed",
          value = vb_pct(s$n_msf, s$n_confirmed)
        ),
        list(
          label = "Of alive at notification",
          value = vb_pct(s$n_msf, s$n_alive_notif)
        )
      ))
    })

    output$deaths <- renderText(scales::number(df_summary()$n_died))

    output$deaths_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(
          label = "CFR",
          value = vb_pct(s$n_died, s$n_died + s$n_recovered)
        )
      ))
    })

    output$nonisolated_deaths <- renderText(
      scales::number(df_summary()$n_nonisolated_deaths)
    )

    output$nonisolated_deaths_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(
          label = "Of deaths",
          value = vb_pct(s$n_nonisolated_deaths, s$n_died)
        )
      ))
    })
  })
}

#* Value-box helpers ------------------------------------

# n / denom as a percentage string, or an em dash when denom is 0 - avoids a
# 0/0 -> NaN reaching the UI.
vb_pct <- function(n, denom) {
  if (denom == 0) "—" else scales::percent(n / denom, accuracy = 0.1)
}

# Muted label / bold value stat line under a value box.
vb_stat_line <- function(stats) {
  do.call(
    tags$span,
    c(
      list(class = "vb-breakdown"),
      lapply(stats, function(s) {
        tags$span(
          class = "vb-stat",
          tags$span(class = "vb-stat-label", paste0(s$label, ":")),
          tags$span(class = "vb-stat-value", s$value)
        )
      })
    )
  )
}

# Value-box title with an info-circle tooltip.
vb_title_info <- function(title, html, max_width = "300px") {
  tip_class <- paste0("tooltip-", gsub("[^a-z]", "", tolower(title)))
  icon <- bslib::tooltip(
    bsicons::bs_icon("info-circle"),
    htmltools::tagList(
      htmltools::tags$style(sprintf(
        ".%s .tooltip-inner { max-width: %s; }",
        tip_class,
        max_width
      )),
      htmltools::div(style = "text-align: left;", htmltools::HTML(html))
    ),
    placement = "top",
    options = list(customClass = tip_class)
  )
  tags$div(
    class = "d-flex align-items-center gap-2",
    title,
    tags$span(class = "vb-tooltip", icon)
  )
}
