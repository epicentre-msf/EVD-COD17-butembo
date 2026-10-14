# De-identify the clean linelist so it can be shared outside the epi team.
# Reads the latest export from Donnees/propre, writes to Donnees/partage.

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))

#* Path ------------------------------------
time_write <- time_stamp()
latest_narr_ll_clean <- fs::dir_ls("local/linelist", regex = "BUT-EVD") |> max()

#* Import data -----------------------------
# latest surveillance linelist
ll_clean <- readRDS(latest_narr_ll_clean)

#* SHARE VARIABLES -------------------------

share_vars <- c(
  "unique_id",
  "age",
  "sex",
  "date_symptom_onset",
  "dead_upon_notif",
  "isolation_site_id",
  "date_admission_eff",
  "isolated_etc",
  "etc_site",
  "type_of_exit",
  "EVD_status",
  "date_exit_eff",
  "death_place"
)

#* SHARE ID --------------------------------
# unique_id is built in 1_prep_data.R and arrives with the clean export
ll_share <- ll_clean |>
  select(all_of(share_vars)) |>
  # the _eff suffix is internal - readers only ever see the one exit date
  rename(date_exit = date_exit_eff)

# last line of defence, in case share_vars ever names a free-text field
nominative <- stringr::str_subset(
  names(ll_share),
  stringr::regex(
    "nom|telephone|^tel|adresse|comments|infector_name",
    ignore_case = TRUE
  )
)

if (length(nominative)) {
  cli::cli_abort(
    "Nominative column{?s} in the share export: {toString(nominative)}"
  )
}

#! Export: Donnees/partage is the de-identified copy, never Donnees/propre
export_clean(
  ll_share,
  CONFIG$export_prefix,
  "linelist-share",
  time_write,
  dir = butembo_share_data_path
)

#* NOMINATIVE CONFIRMED CASES --------------
# One row per positive test. Goes to lab-data, never to the partage folder.
confirmed <- ll_clean |>
  filter(EVD_status == "Confirmed")

cli::cli_inform(
  "{nrow(confirmed)} confirmed of {nrow(ll_clean)} rows; {nrow(ll_clean) - nrow(confirmed)} dropped (not confirmed)"
)

positive_tests <- confirmed |>
  select(
    unique_id,
    contains("lab_id_"),
    contains("lab_result_"),
    contains("date_lab_sample_")
  ) |>
  mutate(across(-unique_id, as.character)) |>
  pivot_longer(
    cols = -unique_id,
    names_to = c(".value", "test"),
    names_pattern = "(.*?)(\\d+)$",
    names_transform = list(test = as.integer)
  ) |>
  rename_with(\(x) str_remove(x, "_$")) |>
  filter(lab_result == "Positif") |>
  mutate(date_lab_sample = as.Date(date_lab_sample)) |>
  # duplicate linelist rows share a unique_id and repeat the same test
  distinct(unique_id, lab_id, date_lab_sample)

# unique_id is shared by linelist rows with the same pid and nom
confirmed_cases <- confirmed |>
  select(unique_id, pid, nom, age, age_unit, sex, date_notification) |>
  slice_head(n = 1, by = unique_id)

cli::cli_inform(
  "{nrow(confirmed) - nrow(confirmed_cases)} duplicate row{?s} collapsed to {nrow(confirmed_cases)} confirmed case{?s} (first row per unique_id)"
)

ll_nominative <- confirmed_cases |>
  left_join(
    positive_tests,
    by = join_by(unique_id),
    relationship = "one-to-many"
  )

cli::cli_inform(c(
  "{nrow(confirmed_cases)} confirmed case{?s} before lab join, {nrow(ll_nominative)} row{?s} after",
  "{sum(!confirmed_cases$unique_id %in% positive_tests$unique_id)} confirmed case{?s} with no positive test",
  "{sum(table(positive_tests$unique_id) > 1)} confirmed case{?s} with more than one positive test",
  "{sum(is.na(positive_tests$lab_id))} of {nrow(positive_tests)} positive test{?s} with no lab_id"
))

export_clean(
  ll_nominative,
  CONFIG$export_prefix,
  "confirmed-nominative",
  time_write,
  dir = lab_data_dir
)

# lab colleagues open this in Excel, not R
rio::export(
  ll_nominative,
  fs::path(
    lab_data_dir,
    glue::glue("{CONFIG$export_prefix}_confirmed-nominative__{time_write}.xlsx")
  )
)
