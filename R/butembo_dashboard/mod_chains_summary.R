# Transmission tab, right column: epicurve by narrative status, then tabs for
# mode of infection, exposures and place of transmission, on the filtered cases.

mod_chains_summary_ui <- function(id) {
  ns <- NS(id)
  # one column: the curve on top, the other analyses under it
  # two equal rows that together fill whatever height the network card takes
  bslib::layout_columns(
    col_widths = 12,
    row_heights = c(1, 1),
    min_height = "600px",
    # time_ui / time_server and the recolour handler come from the main epicurve
    bslib::layout_columns(
      col_widths = 12,
      do.call(
        time_ui,
        c(
          list(
            id = ns("curve"),
            title = "Courbe épidémique",
            date_vars = date_vars,
            group_vars = group_vars,
            date_interval_default = "week",
            group_var_default = "narrative_status"
          ),
          time_ui_labs
        )
      )
    ),
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Type de transmission",
        shiny::uiOutput(ns("type_note")),
        highcharter::highchartOutput(ns("type_chart"), height = "100%")
      ),
      bslib::nav_panel(
        "Expositions",
        shiny::uiOutput(ns("expo_note")),
        highcharter::highchartOutput(ns("expo_chart"), height = "100%")
      ),
      bslib::nav_panel(
        "Formation sanitaire de transmission",
        shiny::uiOutput(ns("place_note")),
        reactable::reactableOutput(ns("place_tbl"))
      )
    )
  )
}

# exposure fields kept for the chart, each Yes / No / Inc.
exposure_vars <- c(
  "Cas d'EVD" = "contact_EVD_case",
  "Funérailles" = "contact_funeral",
  "Voyage" = "contact_travel",
  "Structure de santé" = "contact_HF",
  "Tradipraticien" = "contact_tradi",
  "Contact non humain" = "contact_non_human"
)

mod_chains_summary_server <- function(id, ll) {
  moduleServer(id, function(input, output, session) {
    time_server(
      id = "curve",
      df = ll,
      date_vars = date_vars,
      group_vars = group_vars,
      group_pal = evd_pal,
      na_colour = na_col
    )

    # same palette handling as the main curve, scoped to this module's chart
    observeEvent(input[["curve-group"]], {
      grp <- input[["curve-group"]]
      session$sendCustomMessage(
        "recolour",
        list(
          id = session$ns("curve-chart"),
          na_col = na_col,
          palette = as.list(c(
            group_pals[[grp]][!names(group_pals[[grp]]) %in% na_labels],
            setNames(
              rep(na_col, length(na_labels) + 1),
              c(getOption("epishiny.na.label"), na_labels)
            )
          ))
        )
      )
    })

    # epishiny has no default-date arg, so set it once on startup
    observe({
      updateSelectInput(session, "curve-date", selected = "date_lab_result_1")
    }) |>
      bindEvent(TRUE, once = TRUE)

    n_missing_note <- \(n_out, n_all, what) {
      htmltools::div(
        class = "card-disclaimer",
        sprintf(
          "%s cas sur %s sans %s, non représentés.",
          scales::number(n_out),
          scales::number(n_all),
          what
        )
      )
    }

    # upstream may leave the missing level as the epishiny label, not NA
    unknown_labels <- c(
      "Inconnu",
      "Unknown",
      getOption("epishiny.na.label", "(Manquant)")
    )

    type_counts <- shiny::reactive({
      ll() |>
        dplyr::filter(!is.na(transmission_type_1), !transmission_type_1 %in% unknown_labels) |>
        dplyr::count(type = transmission_type_1) |>
        dplyr::arrange(dplyr::desc(n))
    })

    output$type_note <- shiny::renderUI({
      n_all <- nrow(ll())
      n_missing_note(
        n_all - sum(type_counts()$n),
        n_all,
        "type de transmission connu"
      )
    })

    output$type_chart <- highcharter::renderHighchart({
      d <- type_counts()
      n_total <- sum(d$n)
      cols <- unname(group_pals$transmission_type_1[as.character(d$type)])

      points <- purrr::pmap(
        list(d$type, d$n, cols),
        \(x, y, col) list(name = as.character(x), y = y, color = col, pct = 100 * y / n_total)
      )

      highcharter::highchart() |>
        highcharter::hc_chart(type = "bar") |>
        highcharter::hc_title(text = NULL) |>
        highcharter::hc_xAxis(
          categories = as.list(as.character(d$type)),
          title = list(text = NULL),
          labels = list(style = list(fontSize = "13px"))
        ) |>
        highcharter::hc_yAxis(
          title = list(text = "Nombre de cas"),
          allowDecimals = FALSE
        ) |>
        highcharter::hc_add_series(
          data = points,
          name = "Cas",
          showInLegend = FALSE,
          dataLabels = list(enabled = TRUE)
        ) |>
        highcharter::hc_tooltip(
          pointFormat = sprintf(
            "<b>{point.y}</b> cas ({point.pct:.0f}%% des %s cas renseignés)",
            n_total
          )
        ) |>
        highcharter::hc_credits(enabled = FALSE)
    })

    place_counts <- shiny::reactive({
      ll() |>
        # structure name is the part before the first "|"
        dplyr::mutate(
          place = stringr::str_squish(
            stringr::str_split_i(HF_name_transmission_1, stringr::fixed("|"), 1)
          )
        ) |>
        dplyr::filter(!is.na(place)) |>
        dplyr::count(place, sort = TRUE) |>
        dplyr::mutate(percent = n / sum(n))
    })

    expo_counts <- shiny::reactive({
      d <- ll()
      tibble::tibble(
        expo = names(exposure_vars),
        # tradi holds one lower-case "oui"
        n = vapply(
          exposure_vars,
          \(v) sum(tolower(d[[v]]) %in% c("yes", "oui")),
          integer(1)
        )
      ) |>
        dplyr::arrange(dplyr::desc(n))
    })

    output$expo_note <- shiny::renderUI({
      d <- ll()
      n_none <- sum(rowSums(!is.na(d[unname(exposure_vars)])) == 0)
      htmltools::div(
        class = "card-disclaimer",
        sprintf(
          "%s cas sur %s sans aucune exposition renseignée. Expositions « Oui » ; un cas peut en cumuler plusieurs.",
          scales::number(n_none),
          scales::number(nrow(d))
        )
      )
    })

    output$expo_chart <- highcharter::renderHighchart({
      d <- expo_counts()
      n_all <- nrow(ll())
      points <- purrr::pmap(
        list(d$expo, d$n),
        \(x, y) list(name = x, y = y, pct = 100 * y / n_all)
      )

      highcharter::highchart() |>
        highcharter::hc_chart(type = "bar") |>
        highcharter::hc_title(text = NULL) |>
        highcharter::hc_xAxis(
          categories = as.list(d$expo),
          title = list(text = NULL),
          labels = list(style = list(fontSize = "13px"))
        ) |>
        highcharter::hc_yAxis(
          title = list(text = "Nombre de cas"),
          allowDecimals = FALSE
        ) |>
        highcharter::hc_add_series(
          data = points,
          name = "Cas",
          color = "#2a6f97",
          showInLegend = FALSE,
          dataLabels = list(enabled = TRUE)
        ) |>
        highcharter::hc_tooltip(
          pointFormat = "<b>{point.y}</b> cas ({point.pct:.0f}%)"
        ) |>
        highcharter::hc_credits(enabled = FALSE)
    })

    output$place_note <- shiny::renderUI({
      n_all <- nrow(ll())
      n_missing_note(
        n_all - sum(place_counts()$n),
        n_all,
        "structure de transmission renseignée"
      )
    })

    output$place_tbl <- reactable::renderReactable({
      d <- place_counts()

      reactable::reactable(
        d,
        highlight = TRUE,
        compact = TRUE,
        pagination = FALSE,
        theme = rt_theme(),
        defaultColDef = reactable::colDef(align = "right"),
        columns = list(
          place = reactable::colDef(name = "Formation sanitaire", align = "left"),
          n = reactable::colDef(
            name = "Cas",
            format = reactable::colFormat(separators = TRUE, locales = "fr-FR")
          ),
          percent = reactable::colDef(
            name = "%",
            format = reactable::colFormat(percent = TRUE, digits = 1)
          )
        )
      )
    })
  })
}
