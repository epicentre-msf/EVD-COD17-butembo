# Faceted epicurves by notification (default) or residence health zone or area, on lab confirmation (default), onset or notification date.

EPICURVE_HZ_DATES <- c(
  "Date de confirmation labo" = "date_lab_result_1",
  "Date de début des symptômes" = "date_symptom_onset",
  "Date de notification" = "date_notification"
)

mod_epicurve_hz_ui <- function(id, group_vars) {
  ns <- shiny::NS(id)

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex align-items-center flex-wrap gap-3",
      shiny::textOutput(ns("plot_title"), inline = TRUE),
      shiny::uiOutput(ns("footnote"))
    ),
    bslib::card_body(
      bslib::layout_sidebar(
        fillable = TRUE,
        sidebar = bslib::sidebar(
          width = 250,
          position = "right",
          bg = "#fff",
          shiny::selectInput(
            ns("date_var"),
            "Date",
            choices = EPICURVE_HZ_DATES,
            selected = "date_lab_result_1",
            selectize = FALSE
          ),
          shinyWidgets::radioGroupButtons(
            ns("place_type"),
            "Lieu",
            choices = c(Notification = "notif", "Résidence" = "res"),
            selected = "notif",
            size = "sm"
          ),
          shinyWidgets::radioGroupButtons(
            ns("level"),
            "Niveau",
            choices = c("Zone de santé" = "adm2", "Aire de santé" = "adm3"),
            selected = "adm2",
            size = "sm"
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
          ),
          shiny::selectInput(
            ns("group_var"),
            "Groupe",
            choices = c("Aucun" = "none", group_vars),
            selected = "none",
            selectize = FALSE
          ),
          shiny::selectInput(
            ns("order_by"),
            "Classer les lieux par",
            choices = c(
              "Cas des 7 derniers jours" = "last_7d",
              "Cas des 14 derniers jours" = "last_14d",
              "Cas des 21 derniers jours" = "last_21d",
              "Total des cas" = "total"
            ),
            selected = "total",
            selectize = FALSE
          ),
          shiny::numericInput(
            ns("top_n"),
            "Afficher les N premiers lieux",
            value = 6,
            min = 1,
            max = 50,
            step = 1
          ),
          bslib::input_switch(ns("show_all"), "Afficher tous les lieux", value = FALSE)
        ),
        shiny::tags$div(
          style = "overflow-y: auto; height: 100%;",
          shiny::plotOutput(ns("plot"), height = "100%")
        )
      )
    )
  )
}

# "W42" per tick, with the ISO year beneath the first tick and each year change
label_epiweek <- function(x) {
  yr <- format(x, "%G")
  new_yr <- is.na(dplyr::lag(yr)) | yr != dplyr::lag(yr)
  paste0("W", as.integer(format(x, "%V")), dplyr::if_else(new_yr, paste0("\n", yr), ""))
}

mod_epicurve_hz_server <- function(id, df, group_vars) {
  shiny::moduleServer(id, function(input, output, session) {
    n_cols <- 3L

    place_type <- shiny::reactive(input$place_type %||% "notif")
    place_lab <- shiny::reactive({
      if (identical(place_type(), "res")) "résidence" else "notification"
    })

    date_lab <- shiny::reactive({
      tolower(names(EPICURVE_HZ_DATES)[EPICURVE_HZ_DATES == date_col()])
    })

    date_col <- shiny::reactive(input$date_var %||% "date_lab_result_1")

    output$plot_title <- shiny::renderText({
      paste0("Nouveaux cas par lieu de ", place_lab(), " (", date_lab(), ")")
    })

    # NA dates cannot be placed on the axis
    df_dated <- shiny::reactive({
      lvl <- paste0(
        if (identical(input$level, "adm3")) "adm3" else "adm2",
        "_name__",
        place_type()
      )
      df() |>
        dplyr::filter(!is.na(.data[[date_col()]])) |>
        dplyr::mutate(hz = dplyr::coalesce(as.character(.data[[lvl]]), getOption("epishiny.na.label")))
    })

    output$footnote <- shiny::renderUI({
      n_all <- nrow(df())
      n_dropped <- n_all - nrow(df_dated())
      htmltools::div(
        class = "card-disclaimer",
        paste0(
          n_dropped, " cas sur ", n_all, " (",
          sprintf("%.1f", 100 * n_dropped / max(n_all, 1)), " %) n'ont pas de ", date_lab(), " et ne sont pas affichés."
        )
      )
    })

    group_var <- shiny::reactive({
      if (identical(input$group_var, "none")) NULL else input$group_var
    })

    daily <- shiny::reactive({
      d <- df_dated()
      na_lab <- getOption("epishiny.na.label")
      d$grp <- if (is.null(group_var())) {
        "All"
      } else {
        dplyr::coalesce(as.character(d[[group_var()]]), na_lab)
      }
      dplyr::count(d, hz, grp, date = .data[[date_col()]], name = "n")
    })

    # factor order of the source column, so levels keep their natural order
    grp_levels <- shiny::reactive({
      present <- unique(daily()$grp)
      src <- df_dated()[[group_var()]]
      base <- if (is.factor(src)) levels(src) else sort(unique(as.character(src)))
      c(intersect(base, present), setdiff(present, base))
    })

    selected_zones <- shiny::reactive({
      d <- daily()
      shiny::req(nrow(d) > 0)
      max_date <- max(d$date)

      rank_tbl <- d |>
        dplyr::summarise(
          .by = hz,
          last_7d = sum(n[date >= max_date - 6]),
          last_14d = sum(n[date >= max_date - 13]),
          last_21d = sum(n[date >= max_date - 20]),
          total = sum(n)
        ) |>
        dplyr::arrange(dplyr::desc(.data[[input$order_by]]))

      if (!isTRUE(input$show_all)) {
        rank_tbl <- dplyr::slice_head(rank_tbl, n = max(1L, as.integer(input$top_n)))
      }
      rank_tbl$hz
    })

    output$plot <- shiny::renderPlot(
      {
        zones <- selected_zones()
        d <- daily() |> dplyr::filter(hz %in% zones)
        shiny::req(nrow(d) > 0)

        by_week <- input$interval == "week"
        if (by_week) {
          d <- dplyr::mutate(
            d,
            date = lubridate::floor_date(date, "week", week_start = 1)
          )
        }

        # Latest data week is still filling; Sys.Date() lags the data, so key off the data
        this_week <- lubridate::floor_date(max(d$date), "week", week_start = 1)

        d <- d |>
          dplyr::summarise(.by = c(hz, grp, date), n = sum(n)) |>
          dplyr::mutate(
            hz = factor(hz, levels = zones),
            partial = date >= this_week
          )

        if (is.null(group_var())) {
          fill_scale <- ggplot2::scale_fill_manual(
            values = c("All" = "#9e2a2b"),
            guide = "none"
          )
        } else {
          lv <- grp_levels()
          d$grp <- factor(d$grp, levels = lv)
          fill_scale <- ggplot2::scale_fill_manual(
            values = stats::setNames(group_colours(group_var(), lv), lv),
            name = names(group_vars)[group_vars == group_var()],
            drop = FALSE
          )
        }

        # Weekly bars sit on ISO Mondays, so label them by ISO epiweek
        if (by_week) {
          x_scale <- ggplot2::scale_x_date(
            breaks = scales::breaks_width("4 weeks"),
            labels = label_epiweek
          )
          x_lab <- "Semaine épidémiologique (ISO)"
        } else {
          x_scale <- ggplot2::scale_x_date(labels = scales::label_date_short())
          x_lab <- names(EPICURVE_HZ_DATES)[EPICURVE_HZ_DATES == date_col()]
        }

        ggplot2::ggplot(d, ggplot2::aes(x = date, y = n, fill = grp, alpha = partial)) +
          ggplot2::geom_col(
            col = "grey70",
            linewidth = 0.2,
            width = if (by_week) 7 else 1
          ) +
          fill_scale +
          ggplot2::scale_alpha_manual(
            values = c(`FALSE` = 1, `TRUE` = 0.35),
            guide = "none"
          ) +
          ggplot2::facet_wrap(~hz, ncol = n_cols, scales = input$scales) +
          ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0, 0.08)),
            labels = scales::label_comma()
          ) +
          x_scale +
          ggplot2::theme_minimal(base_size = 14) +
          ggplot2::labs(x = x_lab, y = "Nouveaux cas") +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold"),
            panel.grid.minor = ggplot2::element_blank(),
            panel.spacing.x = ggplot2::unit(1, "lines"),
            legend.position = "bottom"
          )
      },
      height = function() {
        n_rows <- ceiling(length(selected_zones()) / n_cols)
        if (n_rows > 3) n_rows * 200L else "auto"
      }
    )
  })
}
