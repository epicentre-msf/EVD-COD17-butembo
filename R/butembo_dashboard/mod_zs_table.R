# Confirmed cases by health zone (adm2_comptabilisation): last 7 days, last 21 days, total.

mod_zs_table_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    full_screen = TRUE,
    title = htmltools::div(
      class = "d-flex align-items-center flex-wrap gap-3",
      "Cas confirmés par zone de santé",
      shiny::uiOutput(ns("footnote"))
    ),
    bslib::nav_panel(
      "Par date de résultat",
      reactable::reactableOutput(ns("tbl"))
    ),
    bslib::nav_panel(
      "Notifiés par semaine",
      reactable::reactableOutput(ns("tbl_weeks"))
    )
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
    # last 5 ISO weeks including the current one, anchored on today like the epiweek box
    output$tbl_weeks <- reactable::renderReactable({
      this_week <- lubridate::floor_date(Sys.Date(), "week", week_start = 1)
      weeks <- this_week - 7 * 4:0
      wk_ids <- paste0("w", seq_along(weeks))

      d <- df() |>
        dplyr::filter(EVD_status == "Confirmed") |>
        dplyr::mutate(
          zone = dplyr::coalesce(as.character(adm2_comptabilisation), na_lab),
          wk = lubridate::floor_date(
            date_notification,
            "week",
            week_start = 1
          )
        )

      tbl <- tibble::tibble(zone = unique(d$zone))
      for (i in seq_along(weeks)) {
        n_wk <- d |>
          dplyr::filter(wk == weeks[i]) |>
          dplyr::count(zone, name = wk_ids[i])
        tbl <- dplyr::left_join(
          tbl,
          n_wk,
          by = dplyr::join_by(zone),
          relationship = "one-to-one"
        )
      }
      tbl <- tbl |>
        dplyr::mutate(dplyr::across(dplyr::all_of(wk_ids), \(x) {
          dplyr::coalesce(x, 0L)
        })) |>
        dplyr::mutate(total = rowSums(dplyr::pick(dplyr::all_of(wk_ids)))) |>
        dplyr::arrange(dplyr::desc(total))
      shiny::req(nrow(tbl) > 0)

      max_n <- max(unlist(tbl[wk_ids]), 1)
      ramp <- scales::col_numeric(c("#ffffff", "#9e2a2b"), c(0, max_n))
      wk_col <- \(nm, id) {
        reactable::colDef(
          name = nm,
          footer = sum(tbl[[id]]),
          style = \(v) {
            list(
              background = ramp(v),
              color = if (v > 0.6 * max_n) "#ffffff" else "#212529"
            )
          }
        )
      }
      wk_cols <- purrr::map2(
        paste0(
          "S",
          lubridate::isoweek(weeks),
          c(rep("", length(weeks) - 1), " (en cours)")
        ),
        wk_ids,
        wk_col
      ) |>
        rlang::set_names(wk_ids)

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
        columns = c(
          list(
            zone = reactable::colDef(
              name = "Zone de santé",
              align = "left",
              footer = "Total"
            )
          ),
          wk_cols,
          list(
            total = reactable::colDef(name = "Total", footer = sum(tbl$total))
          )
        )
      )
    })
  })
}
