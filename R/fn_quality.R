# Data-quality tables for the dashboard's Data quality tab: variable
# completeness and admin-name-to-pcode match rate. Built once in prep on the
# whole linelist, so the app only renders them.
# Moved from R/butembo_dashboard/mod_quality.R; edit here, not in the app.

GEO_LEVELS_Q <- c("Province" = 1L, "Health Zone" = 2L, "Health Area" = 3L)
GEO_LOC_TYPES <- c(res = "Residence", onset = "Onset", notif = "Notification")

# identifiers/free text excluded from the completeness table - not
# analytical variables
QUALITY_EXCLUDE_VARS <- c("unique_id", "nom", "phone_number")

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

# one row per location type, three columns per admin level: recorded (name
# non-missing, % of all rows), matched (name resolved to a pcode, % of the
# recorded names) and matched % of all rows
geo_match_summary_wide <- function(d) {
  n_all <- nrow(d)
  purrr::map_dfr(names(GEO_LOC_TYPES), function(lt) {
    row <- list(Location = GEO_LOC_TYPES[[lt]])
    for (lvl_lab in names(GEO_LEVELS_Q)) {
      lvl <- GEO_LEVELS_Q[[lvl_lab]]
      name_col <- paste0("adm", lvl, "_name__", lt)
      pcode_col <- paste0("adm", lvl, "_pcode__", lt)
      rec_lab <- paste0(lvl_lab, " recorded %")
      match_lab <- paste0(lvl_lab, " matched % of recorded")
      all_lab <- paste0(lvl_lab, " matched % of all")
      if (!all(c(name_col, pcode_col) %in% names(d))) {
        row[[rec_lab]] <- NA_real_
        row[[match_lab]] <- NA_real_
        row[[all_lab]] <- NA_real_
        next
      }
      recorded <- !is.na(d[[name_col]])
      matched <- recorded & !is.na(d[[pcode_col]])
      row[[rec_lab]] <- 100 * sum(recorded) / n_all
      row[[match_lab]] <- if (any(recorded)) {
        100 * sum(matched) / sum(recorded)
      } else {
        NA_real_
      }
      row[[all_lab]] <- 100 * sum(matched) / n_all
    }
    tibble::as_tibble(row)
  })
}

# distinct recorded name + parent pairs that never resolved to a pcode, most
# frequent first. Parent keeps same-named areas in two zones apart; Reason
# separates a bad name from one that failed because its parent did.
geo_unmatched <- function(d) {
  purrr::map_dfr(names(GEO_LOC_TYPES), function(lt) {
    purrr::map_dfr(names(GEO_LEVELS_Q), function(lvl_lab) {
      lvl <- GEO_LEVELS_Q[[lvl_lab]]
      name_col <- paste0("adm", lvl, "_name__", lt)
      pcode_col <- paste0("adm", lvl, "_pcode__", lt)
      if (!all(c(name_col, pcode_col) %in% names(d))) {
        return(NULL)
      }
      parent_name_col <- paste0("adm", lvl - 1L, "_name__", lt)
      parent_pcode_col <- paste0("adm", lvl - 1L, "_pcode__", lt)
      has_parent <- lvl > 1L && all(c(parent_name_col, parent_pcode_col) %in% names(d))
      d |>
        dplyr::filter(!is.na(.data[[name_col]]), is.na(.data[[pcode_col]])) |>
        dplyr::mutate(
          parent = if (has_parent) .data[[parent_name_col]] else NA_character_,
          reason = if (has_parent) {
            dplyr::if_else(
              is.na(.data[[parent_pcode_col]]),
              "Parent unmatched",
              "Name not in geobase"
            )
          } else {
            "Name not in geobase"
          }
        ) |>
        dplyr::count(
          raw_name = .data[[name_col]],
          parent,
          reason,
          name = "n"
        ) |>
        dplyr::mutate(group = paste0(GEO_LOC_TYPES[[lt]], " – ", lvl_lab)) |>
        dplyr::arrange(dplyr::desc(n)) |>
        dplyr::select(group, raw_name, parent, reason, n)
    })
  })
}

unmatched_display <- function(d) {
  geo_unmatched(d) |>
    dplyr::rename(
      Location = group,
      `Recorded name` = raw_name,
      Parent = parent,
      Reason = reason,
      `N rows` = n
    )
}

# all three tables in the shape mod_quality_server() reads
build_quality <- function(dd) {
  list(
    completeness = completeness_table(dd),
    geo_summary = geo_match_summary_wide(dd),
    unmatched = unmatched_display(dd)
  )
}
