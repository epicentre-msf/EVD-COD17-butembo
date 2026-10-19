# Confirmed cases by health zone (adm2_comptabilisation): last 7 days, last 21 days, total.

mod_zs_table_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "d-flex align-items-center flex-wrap gap-3",
      "Cas confirmés par zone de santé",
      shiny::uiOutput(ns("footnote"))
    ),
    bslib::card_body(reactable::reactableOutput(ns("tbl")))
  )
}

mod_zs_table_server <- function(id, df) {
  shiny::moduleServer(id, function(input, output, session) {
    na_lab <- getOption("epishiny.na.label")

    # windows are cumulative and anchored on the latest result, not Sys.Date()
    summary_tbl <- shiny::reactive({
      d <- df() |>
        dplyr::filter(EVD_status == "Confirmed") |>
        dplyr::mutate(
          zone = dplyr::coalesce(as.character(adm2_comptabilisation), na_lab)
        )
      max_date <- suppressWarnings(max(d$date_lab_result_1, na.rm = TRUE))
      n_since <- \(x, n) sum(x >= max_date - (n - 1), na.rm = TRUE)

      d |>
        dplyr::summarise(
          .by = zone,
          j7 = n_since(date_lab_result_1, 7),
          j21 = n_since(date_lab_result_1, 21),
          total = dplyr::n()
        ) |>
        dplyr::arrange(dplyr::desc(total))
    })

    output$footnote <- shiny::renderUI({
      d <- df() |> dplyr::filter(EVD_status == "Confirmed")
      max_date <- suppressWarnings(max(d$date_lab_result_1, na.rm = TRUE))
      n_undated <- sum(is.na(d$date_lab_result_1))
      htmltools::div(
        class = "card-disclaimer",
        paste0(
          n_undated,
          " cas sans date comptés dans le total uniquement."
        )
      )
    })

    output$tbl <- reactable::renderReactable({
      tbl <- summary_tbl()
      shiny::req(nrow(tbl) > 0)
      tot <- colSums(tbl[c("j7", "j21", "total")])

      # red ramp shared with the Confirmed colour; own domain per column
      count_col <- function(nm, id) {
        ramp <- scales::col_numeric(
          c("#ffffff", "#9e2a2b"),
          c(0, max(tbl[[id]], 1))
        )
        reactable::colDef(
          name = nm,
          footer = tot[[id]],
          style = \(v) {
            bg <- ramp(v)
            dark <- v > 0.6 * max(tbl[[id]], 1)
            list(background = bg, color = if (dark) "#ffffff" else "#212529")
          }
        )
      }

      reactable::reactable(
        tbl,
        highlight = TRUE,
        compact = TRUE,
        pagination = FALSE,
        theme = reactable::reactableTheme(
          style = list(fontSize = "0.82rem"),
          headerStyle = list(fontSize = "0.78rem", fontWeight = 600),
          footerStyle = list(fontWeight = 600),
          cellPadding = "4px 6px"
        ),
        defaultColDef = reactable::colDef(align = "center", minWidth = 60),
        columns = list(
          zone = reactable::colDef(
            name = "Zone de santé",
            align = "left",
            footer = "Total"
          ),
          j7 = count_col("7 j", "j7"),
          j21 = count_col("21 j", "j21"),
          total = count_col("Total", "total")
        )
      )
    })
  })
}
