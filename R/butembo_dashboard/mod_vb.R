# Value boxes above the dashboard: confirmed cases (with MSF caseload), notification status,
# deaths (with non-isolated split), cases investigated.

mod_vb_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(2, 2, 2, 2, 4),
    fill = FALSE,
    class = "pt-2 hgap-tight",
    value_box(
      title = "Semaine épidémiologique",
      value = textOutput(ns("epiweek"), inline = TRUE),
      p(uiOutput(ns("epiweek_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-epiweek",
      height = "85px"
    ),
    value_box(
      title = "Cas confirmés",
      value = textOutput(ns("confirmed"), inline = TRUE),
      p(uiOutput(ns("confirmed_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-confirmed",
      height = "85px"
    ),
    value_box(
      title = "Vivants à la notification",
      value = textOutput(ns("alive_notif"), inline = TRUE),
      p(uiOutput(ns("alive_notif_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-alive",
      height = "85px"
    ),
    value_box(
      title = vb_title_info(
        "Décès",
        paste0(
          "<br>Non isolé : décès hors d'un CTE ou CT, patient arrivé décédé"
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
        "Cas investigués",
        paste0(
          "Est investigué tout cas disposant d'un récit.<br>",
          "Finalisé : récit lu et entièrement encodé dans la base.<br>",
          "En cours : informations clés du récit encodées dans la base.<br>",
          "En attente : récit disponible mais non lu."
        )
      ),
      value = textOutput(ns("narr_final"), inline = TRUE),
      p(uiOutput(ns("narr_info"), inline = TRUE)),
      showcase_layout = "left center",
      class = "vb-accent vb-narrative",
      height = "110px"
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

      n_msf <- sum(!is.na(conf$id_msf))

      n_isolated <- sum(conf$isolated %in% "Yes")

      n_died <- sum(conf$type_of_exit == "Died", na.rm = TRUE)
      n_recovered <- sum(conf$type_of_exit == "Recovered", na.rm = TRUE)

      # deaths with no place or arrival status are left out, so this can undercount
      n_nonisolated_deaths <- sum(
        conf$type_of_exit == "Died" & conf$dead_out_isolation %in% "Yes",
        na.rm = TRUE
      )

      # accent-stripped so "Pre-Traité" and "Pre-Traite" both match
      narr <- chartr("éè", "ee", tolower(stringr::str_squish(d$narratif)))
      investigated <- !(is.na(narr) | narr %in% c("pas dispo", "missing"))
      # "Lu - peu détaillé" counts as read too, whatever dash the export uses
      investigation_status <- dplyr::case_when(
        !investigated ~ NA_character_,
        stringr::str_detect(narr, "^lu($|\\s*[-–—]\\s*peu detaill)") ~
          "Finalised",
        narr == "pre-traite" ~ "Been process",
        narr == "pas lu" ~ "Pending",
        .default = "Unrecognised"
      )
      n_unrecognised <- sum(investigation_status %in% "Unrecognised")
      if (n_unrecognised > 0) {
        message(n_unrecognised, " narratif values not recognised")
      }

      list(
        n_all = nrow(d),
        n_investigated = sum(investigated),
        n_finalised = sum(investigation_status %in% "Finalised"),
        n_in_process = sum(investigation_status %in% "Been process"),
        n_pending = sum(investigation_status %in% "Pending"),
        n_confirmed = n_confirmed,
        n_alive_notif = n_alive_notif,
        n_msf = n_msf,
        n_isolated = n_isolated,
        n_died = n_died,
        n_recovered = n_recovered,
        n_nonisolated_deaths = n_nonisolated_deaths
      )
    })

    # calendar fact, so deliberately blind to the sidebar filters
    wk_start <- lubridate::floor_date(Sys.Date(), "week", week_start = 1)
    output$epiweek <- renderText(paste0(
      "S",
      as.integer(format(wk_start, "%V"))
    ))
    output$epiweek_info <- renderUI({
      mois <- c(
        "janv.",
        "févr.",
        "mars",
        "avr.",
        "mai",
        "juin",
        "juil.",
        "août",
        "sept.",
        "oct.",
        "nov.",
        "déc."
      )
      wk_end <- wk_start + 6
      fmt <- \(d) {
        paste(as.integer(format(d, "%d")), mois[as.integer(format(d, "%m"))])
      }
      paste0(format(wk_start, "%G"), " · ", fmt(wk_start), " – ", fmt(wk_end))
    })

    output$confirmed <- renderText(scales::number(df_summary()$n_confirmed))

    output$confirmed_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(
          label = "Patients MSF",
          value = paste0(
            scales::number(s$n_msf),
            " (",
            vb_pct(s$n_msf, s$n_confirmed),
            ")"
          )
        ),
        list(
          label = "Isolés",
          value = paste0(
            scales::number(s$n_isolated),
            " (",
            vb_pct(s$n_isolated, s$n_confirmed),
            ")"
          )
        )
      ))
    })

    output$alive_notif <-renderText({
      s <- df_summary()
      paste0(
        scales::number(s$n_alive_notif),
        " (",
        vb_pct(s$n_alive_notif, s$n_confirmed),
        ")"
      )
    })

    output$alive_notif_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(
          label = "Patients MSF",
          value = paste0(
            scales::number(s$n_msf),
            " (",
            vb_pct(s$n_msf, s$n_alive_notif),
            ")"
          )
        )
      ))
    })

    output$deaths <- renderText(scales::number(df_summary()$n_died))

    output$deaths_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        # list(
        #   label = "CFR",
        #   value = vb_pct(s$n_died, s$n_died + s$n_recovered)
        # ),
        list(
          label = "Non isolés",
          value = paste0(
            scales::number(s$n_nonisolated_deaths),
            " (",
            vb_pct(s$n_nonisolated_deaths, s$n_died),
            ")"
          )
        )
      ))
    })

    output$narr_final <- renderText({
      s <- df_summary()
      paste0(
        scales::number(s$n_investigated),
        " (",
        vb_pct(s$n_investigated, s$n_all),
        ")"
      )
    })

    output$narr_info <- renderUI({
      s <- df_summary()
      vb_stat_line(list(
        list(
          label = "Finalisés",
          value = paste0(
            scales::number(s$n_finalised),
            " (",
            vb_pct(s$n_finalised, s$n_investigated),
            ")"
          )
        ),
        list(
          label = "En cours",
          value = paste0(
            scales::number(s$n_in_process),
            " (",
            vb_pct(s$n_in_process, s$n_investigated),
            ")"
          )
        ),
        list(
          label = "En attente",
          value = paste0(
            scales::number(s$n_pending),
            " (",
            vb_pct(s$n_pending, s$n_investigated),
            ")"
          )
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
