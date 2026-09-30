# Data quality tab: variable completeness and admin-name-to-pcode match
# rate for residence/onset/notification. Tables are built in prep
# (R/fn_quality.R, saved as app_data$quality); this module only renders them.
# Unlike the rest of the dashboard, not filtered by the sidebar or period.
#
# Tables use reactable, matching evd-2026-app's mod_completeness.R /
# mod_geo_precision.R as closely as this simpler, single-linelist dashboard
# allows: collapsible section rows with an averaged aggregate, the same
# colour ramps, a Rows (Sections/Variables) toggle on the completeness table.

# geo precision severity ramp - exact copy of evd-2026-app's GEO_RAMP_*
# (mod_geo_precision.R): amber->red, interpolated in Lab space by shortfall
# (100 - value), NOT a discrete bin - cells at/near 100% stay almost white,
# only real shortfalls darken
GEO_RAMP_SHORTFALL <- c(1, 5, 10, 20, 35, 50)
GEO_RAMP_COLS <- c("#fcf3d6", "#fbe3a3", "#f6c876", "#efae7a", "#e69484", "#ce7a83")
GEO_RAMP_FN <- grDevices::colorRamp(GEO_RAMP_COLS, space = "Lab")

# completeness ramp - exact copy of evd-2026-app's COMPLETENESS_RAMP /
# COMPLETENESS_RAMP_FG (mod_completeness.R): discrete 5-bin sequential
# green, white text on the two darkest (most complete) bins
COMPLETENESS_RAMP <- c("#edf8e9", "#bae4b3", "#74c476", "#31a354", "#006d2c")
COMPLETENESS_RAMP_FG <- c("#212529", "#212529", "#212529", "#ffffff", "#ffffff")
COMPLETENESS_DASH_COLOR <- "#71797E"

# muted explainer block above a table, bullets with bold terms - mirrors
# GEO_SUMMARY_EXPLAINER / GEO_UNMATCHED_EXPLAINER's style in evd-2026-app
quality_explainer <- function(...) {
  htmltools::div(class = "text-muted small pt-2 mb-3", ...)
}

# exact copy of evd-2026-app's rt_theme() (R/utils.R) - compact/regular font
# size pair + matching header weight and cell padding, shared by every table
rt_theme <- function(size = c("regular", "compact"), cell_padding = NULL) {
  size <- match.arg(size)
  fs <- switch(
    size,
    compact = list(cell = "0.82rem", header = "0.80rem", pad = "4px 6px"),
    regular = list(cell = "0.88rem", header = "0.85rem", pad = "6px 10px")
  )
  reactable::reactableTheme(
    style = list(fontSize = fs$cell),
    headerStyle = list(fontSize = fs$header, fontWeight = 600),
    cellPadding = if (is.null(cell_padding)) fs$pad else cell_padding
  )
}


# mod_quality_ui/server take the pre-built quality tables (a plain list, not a
# reactive) - this tab is independent of the dashboard's sidebar/period filters
mod_quality_ui <- function(id, quality) {
  ns <- NS(id)
  n_unmatched <- nrow(quality$unmatched)

  nav_panel(
    title = tags$span(bsicons::bs_icon("clipboard2-check"), "Data quality"),
    value = id,
    bslib::as_fill_carrier(htmltools::div(
      class = "reporting-container quality-container",
      navset_card_tab(
        id = ns("tabs"),
        nav_panel(
          title = tags$span(bsicons::bs_icon("ui-checks-grid"), "Completeness"),
          value = "completeness",
          bslib::as_fill_carrier(htmltools::div(
            class = "mt-3",
            quality_explainer(
              "Percentage of records with a value filled in for each ",
              "variable, across the whole surveillance linelist. Collapsed ",
              "section rows show the average completeness of the section."
            ),
            htmltools::div(
              class = "epicurve-grey-toggle mb-2",
              shinyWidgets::radioGroupButtons(
                ns("rows"),
                "Rows",
                choices = c("Sections" = "section", "Variables" = "variable"),
                selected = "variable",
                size = "sm"
              )
            ),
            reactable::reactableOutput(ns("completeness_table"), height = "100%")
          ))
        ),
        nav_panel(
          title = tags$span(bsicons::bs_icon("geo-alt"), "Geographic precision"),
          value = "geo",
          bslib::as_fill_carrier(htmltools::div(
            navset_underline(
              nav_panel(
                title = "Match rate",
                value = "geo_summary",
                bslib::as_fill_carrier(htmltools::div(
                  class = "mt-3",
                  quality_explainer(
                    tags$p(
                      class = "mb-0",
                      "Each cell shows the percentage of patients whose place ",
                      "of residence, onset or notification is known at the ",
                      "given admin level."
                    ),
                    tags$ul(
                      class = "mb-0",
                      tags$li(HTML(
                        "<b>Recorded</b>: a location was entered in the linelist"
                      )),
                      tags$li(HTML(paste0(
                        "<b>Matched</b>: the recorded location matched a valid ",
                        "unit in the admin boundary geobase, so it can be mapped"
                      )))
                    )
                  ),
                  reactable::reactableOutput(ns("geo_summary_table"), height = "100%")
                ))
              ),
              nav_panel(
                title = htmltools::tagList(
                  "Unmatched locations",
                  htmltools::span(
                    class = "badge rounded-pill text-bg-secondary ms-1",
                    n_unmatched
                  )
                ),
                value = "geo_unmatched",
                bslib::as_fill_carrier(htmltools::div(
                  class = "mt-3",
                  htmltools::div(
                    class = "d-flex align-items-start gap-5",
                    quality_explainer(
                      "Recorded location names that never matched a valid unit ",
                      "in the admin boundary geobase - a misspelling, or a unit ",
                      "missing from the geobase."
                    ),
                    htmltools::div(
                      class = "ms-auto pt-2 flex-shrink-0",
                      bslib::tooltip(
                        shiny::downloadButton(
                          ns("download_unmatched"),
                          "Download .csv",
                          class = "btn-sm btn-link text-muted text-decoration-none p-0 border-0"
                        ),
                        "Downloads the full unmatched-locations table"
                      )
                    )
                  ),
                  reactable::reactableOutput(ns("geo_unmatched_table"), height = "100%")
                ))
              )
            )
          ))
        )
      )
    ))
  )
}

mod_quality_server <- function(id, quality) {
  moduleServer(id, function(input, output, session) {
    output$completeness_table <- reactable::renderReactable({
      df_tbl <- quality$completeness
      # first variable of each section stands in for the section's group row
      # while expanded (see the aggregated cell renderers below), so its own
      # leaf row is hidden via rowStyle rather than shown twice
      df_tbl$first_var <- !duplicated(df_tbl$section)

      # sections are always grouped - the Rows toggle only sets whether they
      # start collapsed (aggregate row only) or expanded, matching
      # evd-2026-app's mod_completeness.R exactly
      reactable::reactable(
        df_tbl,
        groupBy = "section",
        defaultExpanded = identical(input$rows, "variable"),
        compact = TRUE,
        highlight = TRUE,
        pagination = FALSE,
        theme = rt_theme("compact", cell_padding = "3px 6px"),
        defaultColDef = reactable::colDef(align = "center", headerVAlign = "bottom"),
        rowStyle = reactable::JS(
          "function(rowInfo) {
            if (rowInfo && rowInfo.level > 0 && rowInfo.row.first_var) {
              return { display: 'none' }
            }
          }"
        ),
        columns = list(
          section = reactable::colDef(
            name = "Section",
            align = "left",
            minWidth = 180,
            sticky = "left",
            sortable = FALSE,
            grouped = reactable::JS("function(cellInfo) { return cellInfo.value }"),
            style = list(fontWeight = 600)
          ),
          variable = reactable::colDef(
            name = "Variable",
            align = "left",
            minWidth = 220,
            sticky = "left",
            sortable = FALSE,
            class = "rt-col-divider",
            headerClass = "rt-col-divider",
            cell = reactable::JS(
              "function(cellInfo) {
                return React.createElement('div',
                  { className: 'ct-varname', title: cellInfo.value }, cellInfo.value)
              }"
            ),
            aggregated = reactable::JS(
              "function(cellInfo) {
                if (cellInfo.expanded) {
                  var v = cellInfo.subRows[0].variable
                  return React.createElement('div',
                    { className: 'ct-varname', title: v }, v)
                }
                var n = cellInfo.subRows.length
                return n + (n === 1 ? ' variable' : ' variables')
              }"
            )
          ),
          first_var = reactable::colDef(show = FALSE),
          pct_complete = reactable::colDef(
            name = "% complete",
            width = 100,
            aggregate = "mean",
            cell = completeness_pct_cell_js(),
            aggregated = completeness_pct_cell_js(aggregated = TRUE)
          )
        )
      )
    })

    output$geo_summary_table <- reactable::renderReactable({
      wide <- quality$geo_summary
      pct_cols <- setdiff(names(wide), "Location")
      pct_col_defs <- stats::setNames(
        lapply(pct_cols, function(cn) reactable::colDef(cell = geo_pct_cell)),
        pct_cols
      )
      reactable::reactable(
        wide,
        compact = TRUE,
        highlight = TRUE,
        pagination = FALSE,
        theme = rt_theme(),
        defaultColDef = reactable::colDef(align = "center", headerVAlign = "bottom"),
        columns = c(
          list(Location = reactable::colDef(align = "left")),
          pct_col_defs
        )
      )
    })

    output$geo_unmatched_table <- reactable::renderReactable({
      u <- quality$unmatched
      reactable::reactable(
        u,
        groupBy = "Location",
        compact = TRUE,
        highlight = TRUE,
        defaultPageSize = 15,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(15, 30, 100),
        defaultSorted = list(`N rows` = "desc"),
        searchable = TRUE,
        theme = rt_theme(cell_padding = "5px 10px"),
        defaultColDef = reactable::colDef(headerVAlign = "bottom")
      )
    })

    output$download_unmatched <- shiny::downloadHandler(
      filename = function() paste0("unmatched-locations-", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(quality$unmatched, file, row.names = FALSE)
      }
    )
  })
}

#* helpers ------------------------------------

# exact copy of evd-2026-app's completeness_pct_cell_js() (mod_completeness.R):
# NA -> em dash, exactly 0 -> muted (0% is signal, not missing, so no pill),
# else a rounded-% chip on the 5-bin green ramp. `aggregated = TRUE` adds the
# expanded-state swap: a collapsed section shows its aggregate mean (the
# value reactable already computed); an expanded one shows its first
# variable's value instead (the group row stands in for that hidden leaf row)
completeness_pct_cell_js <- function(aggregated = FALSE) {
  reactable::JS(sprintf(
    "function(cellInfo) {
      var v = cellInfo.value
      %s
      if (v == null || isNaN(v)) return '—'
      var t = Math.round(v)
      if (t === 0) return React.createElement('span',
        { style: { color: '%s', fontWeight: 300 } }, '0%%')
      var bg = %s
      var fg = %s
      var i = Math.min(Math.floor(v / 20), 4)
      return React.createElement('div', { style: {
        background: bg[i], color: fg[i], fontWeight: 500,
        borderRadius: '3px', margin: '-3px -4px', padding: '3px 4px'
      }}, t + '%%')
    }",
    if (aggregated) "if (cellInfo.expanded) v = cellInfo.subRows[0][cellInfo.column.id]" else "",
    COMPLETENESS_DASH_COLOR,
    jsonlite::toJSON(COMPLETENESS_RAMP),
    jsonlite::toJSON(COMPLETENESS_RAMP_FG)
  ))
}

# exact copy of evd-2026-app's geo_pct_cell() (mod_geo_precision.R): a cell
# that rounds to 100% renders as plain text (nothing to flag); anything
# below wears a pill whose fill is interpolated from the shortfall
# (100 - value) along the Lab-space amber->red ramp
geo_pct_cell <- function(value) {
  if (is.na(value)) {
    return("—")
  }
  d <- 100 - round(value)
  if (d < 1) {
    return(sprintf("%.0f%%", value))
  }
  pos <- stats::approx(
    GEO_RAMP_SHORTFALL,
    seq(0, 1, length.out = length(GEO_RAMP_SHORTFALL)),
    xout = d,
    rule = 2
  )$y
  htmltools::div(
    style = sprintf(
      "background: %s; font-weight: 600; border-radius: 3px; margin: -3px -2px; padding: 3px 6px;",
      grDevices::rgb(GEO_RAMP_FN(pos), maxColorValue = 255)
    ),
    title = sprintf("Exact value: %.1f%%", value),
    sprintf("%.0f%%", value)
  )
}
