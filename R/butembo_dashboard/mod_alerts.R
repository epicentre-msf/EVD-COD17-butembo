# Alerts tab: stacked daily or weekly bars per health zone, filled by the chosen grouping.
# Alerts are aggregated counts, so the global period and sidebar filters do not apply.

# Each grouping lists the count columns that stack; "rest" is total minus their sum.
ALERT_GROUPS <- list(
  vital = list(
    lab = "Statut vital",
    cols = c(Vivant = "alert_alive", `Décédé` = "alert_dead"),
    cols_col = c(Vivant = "#2a6f97", `Décédé` = "#9e2a2b"),
    prop_lvl = "Vivant",
    prop_lab = "% vivants"
  ),
  valid = list(
    lab = "Statut de validation",
    cols = c(`Validée` = "valid", `Invalidée` = "invalid"),
    cols_col = c(`Validée` = "#2a9d8f", `Invalidée` = "#e9a03b"),
    rest = "En attente ou non classée",
    prop_lvl = "Validée",
    prop_lab = "% validées"
  ),
  source = list(
    lab = "Source de l'alerte",
    cols = c(
      "Formation sanitaire" = "alert_source_fosa",
      "Recherche active" = "alert_source_ra",
      "Communauté" = "alert_source_comm",
      "Point d'entrée" = "alert_source_poc"
    ),
    cols_col = c(
      "Formation sanitaire" = "#2a6f97",
      "Recherche active" = "#e9a03b",
      "Communauté" = "#2a9d8f",
      "Point d'entrée" = "#7b5ea7"
    ),
    rest = "Autre ou non précisée",
    prop_lvl = "Communauté",
    prop_lab = "% communauté"
  )
)

mod_alerts_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "Alertes",
    value = "alerts",
    bslib::card(
      full_screen = TRUE,
      min_height = 800,
      bslib::card_header("Alertes par zone de santé"),
      bslib::card_body(
        bslib::layout_sidebar(
          fillable = TRUE,
          sidebar = bslib::sidebar(
            width = 250,
            position = "right",
            bg = "#fff",
            shiny::selectInput(
              ns("group"),
              "Grouper par",
              choices = stats::setNames(
                names(ALERT_GROUPS),
                vapply(ALERT_GROUPS, `[[`, character(1), "lab")
              ),
              selected = "vital",
              selectize = FALSE
            ),
            shinyWidgets::radioGroupButtons(
              ns("interval"),
              "Intervalle de temps",
              choices = c("Jour" = "day", "Semaine" = "week"),
              selected = "week",
              size = "sm"
            ),
            shinyWidgets::radioGroupButtons(
              ns("scales"),
              "Échelles de l'axe Y",
              choices = c("Fixes" = "fixed", "Libres" = "free_y"),
              selected = "fixed",
              size = "sm"
            )
          ),
          shiny::tags$div(
            style = "overflow-y: auto; height: 100%;",
            shiny::plotOutput(ns("plot"), height = "100%")
          )
        )
      )
    )
  )
}

mod_alerts_server <- function(id, alerts) {
  shiny::moduleServer(id, function(input, output, session) {
    n_cols <- 3L

    grp <- shiny::reactive(ALERT_GROUPS[[input$group %||% "vital"]])

    # Long format: one row per zone, date and stack level
    long <- shiny::reactive({
      g <- grp()
      d <- alerts |>
        dplyr::mutate(
          alert_total = alert_alive + alert_dead,
          valid = valid_alive + valid_dead,
          invalid = invalid_alive + invalid_dead
        )

      out <- purrr::imap(
        g$cols,
        \(col, lvl) {
          dplyr::summarise(
            d,
            .by = c(health_zone, date),
            n = sum(.data[[col]]),
            level = lvl
          )
        }
      ) |>
        dplyr::bind_rows()

      if (!is.null(g$rest)) {
        rest <- d |>
          dplyr::mutate(
            n = pmax(
              0L,
              alert_total - rowSums(dplyr::pick(dplyr::all_of(unname(g$cols))))
            )
          ) |>
          dplyr::summarise(.by = c(health_zone, date), n = sum(n)) |>
          dplyr::mutate(level = g$rest)
        out <- dplyr::bind_rows(out, rest)
      }
      out
    })

    weekly <- shiny::reactive(identical(input$interval, "week"))

    plot_data <- shiny::reactive({
      g <- grp()
      d <- long()
      if (weekly()) {
        d <- dplyr::mutate(d, date = lubridate::floor_date(date, "week", week_start = 1))
      }
      d |>
        dplyr::summarise(.by = c(health_zone, level, date), n = sum(n)) |>
        dplyr::mutate(
          health_zone = stringr::str_to_title(health_zone),
          level = factor(level, levels = c(names(g$cols), g$rest))
        )
    })

    output$plot <- shiny::renderPlot(
      {
        g <- grp()
        d <- plot_data()
        shiny::req(nrow(d) > 0)

        fill_cols <- c(g$cols_col, stats::setNames(na_col, g$rest))

        x_lab <- if (weekly()) "Semaine épidémiologique (ISO)" else "Date"

        # Zone-periods with no alerts have no proportion, so they leave a gap in the line
        prop_d <- d |>
          dplyr::summarise(
            .by = c(health_zone, date),
            total = sum(n),
            num = sum(n[level == g$prop_lvl])
          ) |>
          dplyr::filter(total > 0) |>
          dplyr::mutate(prop = num / total)

        # One scale factor for all facets: sec_axis cannot differ per panel
        k <- max(prop_d$total, 1)

        ggplot2::ggplot(d, ggplot2::aes(x = date, y = n, fill = level)) +
          geom_epicurve(weekly()) +
          ggplot2::geom_line(
            data = prop_d,
            ggplot2::aes(x = date, y = prop * k, group = health_zone),
            inherit.aes = FALSE,
            colour = "grey15",
            linewidth = 0.7
          ) +
          ggplot2::geom_point(
            data = prop_d,
            ggplot2::aes(x = date, y = prop * k),
            inherit.aes = FALSE,
            colour = "grey15",
            size = 1.2
          ) +
          ggplot2::scale_fill_manual(values = fill_cols, name = g$lab, drop = FALSE) +
          ggplot2::facet_wrap(~health_zone, ncol = n_cols, scales = input$scales) +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.08)),
            labels = scales::label_comma(),
            sec.axis = ggplot2::sec_axis(
              ~ . / k,
              name = g$prop_lab,
              breaks = seq(0, 1, by = 0.1),
              labels = scales::label_percent()
            )
          ) +
          scale_x_epicurve(weekly()) +
          ggplot2::labs(x = x_lab, y = "Alertes") +
          theme_epicurve()
      },
      height = function() {
        n_rows <- ceiling(dplyr::n_distinct(plot_data()$health_zone) / n_cols)
        if (n_rows > 3) n_rows * 200L else "auto"
      }
    )
  })
}
