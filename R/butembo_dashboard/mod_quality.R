# Data quality tab: variable completeness and admin-name-to-pcode match
# rate for residence/onset/notification, computed live from the linelist -
# unlike the rest of the dashboard, not filtered by the sidebar or period.
#
# Tables use reactable, matching evd-2026-app's mod_completeness.R /
# mod_geo_precision.R as closely as this simpler, single-linelist dashboard
# allows: collapsible section rows with an averaged aggregate, the same
# colour ramps, a Rows (Sections/Variables) toggle on the completeness table.

GEO_LEVELS_Q <- c("Province" = 1L, "Health Zone" = 2L, "Health Area" = 3L)
GEO_LOC_TYPES <- c(res = "Residence", onset = "Onset", notif = "Notification")

# identifiers/free text excluded from the completeness table - not
# analytical variables
QUALITY_EXCLUDE_VARS <- c("unique_id", "nom", "phone_number")

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

# hand-built variable -> section mapping. evd-2026-app sources this from an
# Excel data dictionary; Butembo has no such dictionary, so this is
# maintained here instead - unmapped variables fall into "Other" rather than
# erroring, so a new/renamed column never breaks the table.
QUALITY_SECTION_ORDER <- c(
  "Identification",
  "Demographics",
  "Dates and delays",
  "Clinical",
  "Laboratory",
  "Transmission link",
  "Exposure",
  "Surveillance",
  "Location - residence",
  "Location - onset",
  "Location - notification",
  "Vaccination",
  "Health Facilities",
  "Symptoms"
)

# guessed column names are flagged inline - check these against the real
# linelist and adjust if they don't match
quality_section <- function(v) {
  dplyr::case_when(
    v %in% c(
      "pid", "EVD_status", "id_msf", "id_msf_2", "adm2_pcode__comptabilisation"
    ) ~
      "Identification",
    v %in% c(
      "sex", "age", "age_raw", "age_unit", "age_group", "job", "hcw",
      "pregnant" # guessed column name - confirm
    ) ~
      "Demographics",
    # health-facility-visit dates are their own section below, checked before
    # the generic date_/delay_ pattern so they aren't caught here instead
    grepl("^(HF_name_visited_|date_(start|end)_HF_visited_)[12]$", v) ~
      "Health Facilities",
    v %in% c(
      "pain_eyes_sensitivity_light", "bleeding_urine", "jaundice",
      "bleeding_vomito_negro", "skin_rash", "hichups", "sorethroat",
      "bleeding_vomit", "bleeding_vagina", "conjunctivitis",
      "confused_disoriented", "bleeding_injection_site",
      "bleeding_epistaxis", "breastfeeding", "temp",
      "hematomes_petechies_purpura", "bleeding_gum", "swallowing_problem",
      "bleeding_melenas", "coma", "chest_pain", "breathlessness", "cough",
      "bone_muscle_pain", "joint_pain", "bleeding", "abdominal_pain",
      "diarrhoea", "headache", "loss_of_appetite", "nausea",
      "asthenia_weakness", "fever"
    ) ~
      "Symptoms",
    v %in% c(
      "date_symptom_onset", "date_notification", "date_admission_eff",
      "date_exit_eff"
    ) |
      grepl("^delay_", v) ~
      "Dates and delays",
    v %in% c(
      "dead_upon_notif", "isolated_etc", "etc_site", "death_place",
      "type_of_exit", "outcome"
    ) ~
      "Clinical",
    v %in% c(
      paste0("lab_id_", 1:2),
      paste0("date_lab_result_", 1:2),
      paste0("lab_result_", 1:2),
      paste0("date_lab_sample_", 1:2),
      paste0("sample_type_", 1:2),
      paste0("lab_provenance_", 1:2)
    ) ~
      "Laboratory",
    v %in% c("transmission_type_1", "infector_id_1", "infector_name_1") ~
      "Transmission link",
    v %in% c(
      "contact_non_human", "contact_funeral_body", "contact_EVD_objects",
      "contact_EVD_body_fluids", "contact_EVD_physical", "contact_EVD_house",
      "contact_tradi", "contact_travel", "contact_HF", "contact_funeral",
      "contact_EVD_case"
    ) ~
      "Exposure",
    v %in% c(
      "narratif", # guessed column name - confirm
      "infection_butembo", "contact_tracing_followed_yn",
      "contact_tracing_known_yn"
    ) ~
      "Surveillance",
    grepl("__res$", v) ~ "Location - residence",
    grepl("__onset$", v) ~ "Location - onset",
    grepl("adm", v, ignore.case = TRUE) & grepl("__notif$", v) ~
      "Location - notification",
    v %in% c("vaccination_rvsv_yn", "year_vaccination_rvsv") ~
      "Vaccination",
    .default = NA_character_ # unmatched variables are dropped, not shown
  )
}

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

# mod_quality_ui/server take the static, unfiltered linelist directly (not a
# reactive) - like evd-2026-app's ll_completeness/ll_geo_matching, this tab
# is independent of the dashboard's sidebar/period filters
mod_quality_ui <- function(id, df) {
  ns <- NS(id)
  n_unmatched <- nrow(geo_unmatched(df))

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

mod_quality_server <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    output$completeness_table <- reactable::renderReactable({
      df_tbl <- completeness_table(df)
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
      wide <- geo_match_summary_wide(df)
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
      u <- unmatched_display(df)
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
        utils::write.csv(unmatched_display(df), file, row.names = FALSE)
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

# trailing test number (1/2/3) for Laboratory variables, so they order by
# test rather than by completeness
lab_test_num <- function(v) as.integer(sub(".*_([12])$", "\\1", v))

# category rank so each test slot reads lab_id -> result -> sample ->
# provenance -> date, left to right
lab_category_rank <- function(v) {
  dplyr::case_when(
    grepl("lab_id", v, ignore.case = TRUE) ~ 1L,
    grepl("result", v, ignore.case = TRUE) ~ 2L,
    grepl("sample", v, ignore.case = TRUE) ~ 3L,
    grepl("provenance", v, ignore.case = TRUE) ~ 4L,
    grepl("date", v, ignore.case = TRUE) ~ 5L,
    .default = 6L
  )
}

# % complete per variable, across the whole linelist, grouped into sections
completeness_table <- function(dd) {
  vars <- setdiff(names(dd), QUALITY_EXCLUDE_VARS)
  # only the first two health-facility-visit repeat-group slots are shown
  vars <- vars[!grepl("^(HF_name_visited_|date_(start|end)_HF_visited_)[3-9]", vars)]
  n <- nrow(dd)
  tbl <- tibble::tibble(
    section = factor(quality_section(vars), levels = QUALITY_SECTION_ORDER),
    variable = vars,
    pct_complete = vapply(vars, \(v) 100 * sum(!is.na(dd[[v]])) / n, numeric(1))
  )
  # variables that don't match any named section are dropped, not bucketed
  # into an "Other" section - factor() already turned them to NA since
  # "Other" isn't in QUALITY_SECTION_ORDER
  tbl <- dplyr::filter(tbl, !is.na(section))
  is_lab <- tbl$section == "Laboratory"
  tbl$lab_test_num <- NA_integer_
  tbl$lab_category_rank <- NA_integer_
  tbl$lab_test_num[is_lab] <- lab_test_num(tbl$variable[is_lab])
  tbl$lab_category_rank[is_lab] <- lab_category_rank(tbl$variable[is_lab])
  tbl |>
    dplyr::arrange(section, lab_test_num, lab_category_rank, pct_complete) |>
    dplyr::select(section, variable, pct_complete)
}

# one row per location type, paired "recorded %"/"matched %" columns per
# admin level - recorded = name non-missing, matched = name resolved to a
# pcode, both out of all rows (not just the recorded ones)
geo_match_summary_wide <- function(d) {
  n_all <- nrow(d)
  purrr::map_dfr(names(GEO_LOC_TYPES), function(lt) {
    row <- list(Location = GEO_LOC_TYPES[[lt]])
    for (lvl_lab in names(GEO_LEVELS_Q)) {
      lvl <- GEO_LEVELS_Q[[lvl_lab]]
      name_col <- paste0("adm", lvl, "_name__", lt)
      pcode_col <- paste0("adm", lvl, "_pcode__", lt)
      rec_lab <- paste0(lvl_lab, " recorded %")
      match_lab <- paste0(lvl_lab, " matched %")
      if (!all(c(name_col, pcode_col) %in% names(d))) {
        row[[rec_lab]] <- NA_real_
        row[[match_lab]] <- NA_real_
        next
      }
      recorded <- !is.na(d[[name_col]])
      matched <- recorded & !is.na(d[[pcode_col]])
      row[[rec_lab]] <- 100 * sum(recorded) / n_all
      row[[match_lab]] <- 100 * sum(matched) / n_all
    }
    tibble::as_tibble(row)
  })
}

# distinct recorded names that never resolved to a pcode, most frequent first
geo_unmatched <- function(d) {
  purrr::map_dfr(names(GEO_LOC_TYPES), function(lt) {
    purrr::map_dfr(names(GEO_LEVELS_Q), function(lvl_lab) {
      lvl <- GEO_LEVELS_Q[[lvl_lab]]
      name_col <- paste0("adm", lvl, "_name__", lt)
      pcode_col <- paste0("adm", lvl, "_pcode__", lt)
      if (!all(c(name_col, pcode_col) %in% names(d))) {
        return(NULL)
      }
      d |>
        dplyr::filter(!is.na(.data[[name_col]]), is.na(.data[[pcode_col]])) |>
        dplyr::count(raw_name = .data[[name_col]], name = "n") |>
        dplyr::mutate(group = paste0(GEO_LOC_TYPES[[lt]], " – ", lvl_lab)) |>
        dplyr::arrange(dplyr::desc(n)) |>
        dplyr::select(group, raw_name, n)
    })
  })
}

unmatched_display <- function(d) {
  geo_unmatched(d) |>
    dplyr::rename(Location = group, `Recorded name` = raw_name, `N rows` = n)
}
