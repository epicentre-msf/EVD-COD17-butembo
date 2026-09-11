# Value boxes above the dashboard: confirmed cases, deaths, recoveries.
# Mirrors evd-2026-app's mod_vb.R, cut to three boxes and no CFR panel.

mod_vb_ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1 / 3,
    fill = FALSE,
    class = "pt-2 hgap-tight",
    value_box(
      title = vb_title_info(
        "Confirmed cases",
        paste0(
          "Case classification of the remaining patients:<br><br>",
          "P. = Probable<br>",
          "S. = Suspect"
        ),
        max_width = "260px",
        # only explains the abbreviated tier, so drop it once labels spell out
        hide_when_wide = TRUE
      ),
      value = textOutput(ns("confirmed"), inline = TRUE),
      p(uiOutput(ns("confirmed_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-confirmed",
      height = "85px"
    ),
    value_box(
      title = vb_title_info("Deaths", vb_outcome_tooltip("died")),
      value = textOutput(ns("deaths"), inline = TRUE),
      p(uiOutput(ns("deaths_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-danger",
      height = "85px"
    ),
    value_box(
      title = vb_title_info("Recovered", vb_outcome_tooltip("recovered")),
      value = textOutput(ns("recovered"), inline = TRUE),
      p(uiOutput(ns("recovered_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-success",
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
      # all three headline figures are confirmed cases only
      conf <- dplyr::filter(d, EVD_status == "Confirmed")
      list(
        n_confirmed = nrow(conf),
        n_probable = sum(d$EVD_status == "Probable", na.rm = TRUE),
        n_suspect = sum(d$EVD_status == "Suspect", na.rm = TRUE),
        n_died = sum(conf$type_of_exit == "Died", na.rm = TRUE),
        n_recovered = sum(conf$type_of_exit == "Recovered", na.rm = TRUE)
      )
    })

    output$confirmed <- renderText(scales::number(df_summary()$n_confirmed))

    output$confirmed_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(
          full = "Probable:",
          med = "Prob.",
          short = "P.",
          value = scales::number(s$n_probable)
        ),
        list(
          full = "Suspect:",
          med = "Susp.",
          short = "S.",
          value = scales::number(s$n_suspect)
        )
      ))
    })

    output$deaths <- renderText(scales::number(df_summary()$n_died))

    output$deaths_info <- renderUI({
      s <- df_summary()
      vb_share_line(s$n_died, s$n_confirmed)
    })

    output$recovered <- renderText(scales::number(df_summary()$n_recovered))

    output$recovered_info <- renderUI({
      s <- df_summary()
      vb_share_line(s$n_recovered, s$n_confirmed)
    })
  })
}

#* Value-box helpers ------------------------------------

# Denominator is every confirmed case, including those still under care, so
# the share is not a CFR - say so rather than let it be read as one.
vb_outcome_tooltip <- function(outcome) {
  paste0(
    "Confirmed cases only.<br><br>",
    "The percentage is the share of all confirmed cases that have ",
    outcome,
    ", including those still under care or with no recorded exit. ",
    "It is not a case fatality ratio."
  )
}

# Muted label / bold value stat line under a value box, with the label
# shortened in three tiers as the box narrows (see www/styles.css).
vb_stat_line <- function(stats) {
  do.call(
    tags$span,
    c(
      list(class = "vb-breakdown"),
      lapply(stats, function(s) {
        tags$span(
          class = "vb-stat",
          tags$span(
            class = "vb-stat-label",
            tags$span(class = "lbl-full", s$full),
            tags$span(class = "lbl-med", s$med %||% s$short),
            tags$span(class = "lbl-short", s$short)
          ),
          tags$span(class = "vb-stat-value", s$value)
        )
      })
    )
  )
}

vb_share_line <- function(n, denom) {
  share <- if (denom == 0) {
    "—"
  } else {
    scales::percent(n / denom, accuracy = 0.1)
  }
  vb_stat_line(list(
    list(full = "Of confirmed:", med = "Of conf.:", short = "%", value = share)
  ))
}

# Value-box title with an info-circle tooltip. hide_when_wide drops the icon
# once the box is wide enough to spell the labels out in full.
vb_title_info <- function(
  title,
  html,
  max_width = "300px",
  hide_when_wide = FALSE
) {
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
    tags$span(
      class = if (hide_when_wide) "vb-status-tooltip" else "vb-tooltip",
      icon
    )
  )
}
