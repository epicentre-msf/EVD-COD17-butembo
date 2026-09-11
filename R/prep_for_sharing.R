# De-identify the clean linelist so it can be shared outside the epi team.
# Reads the latest export from Donnees/propre, writes to Donnees/partage.

#* TODO ------------------------------------
#

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))

#* Path ------------------------------------
time_write <- time_stamp()

#* Import data -----------------------------
ll_clean <- readRDS(latest_narr_ll_clean)
date_valid <- clean_file_date(latest_narr_ll_clean)

#* SHARE VARIABLES -------------------------
# A whitelist, not a drop-list: a new column in the raw export stays out until
# it is named here, so no identifier can reach the shared file unnoticed.
share_vars <- c(
  "unique_id",
  # person - age is in years, whatever unit it was entered in
  "age",
  "sex",
  # care pathway
  "date_symptom_onset",
  "dead_upon_notif",
  "isolation_site_id",
  "date_admission_eff",
  # derived in 1_prep_data.R, not raw fields
  "isolated_etc",
  "etc_site",
  # outcome
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

cli::cli_alert_info(
  "{nrow(ll_share)} rows under {n_distinct(ll_share$unique_id)} unique_id, \\
   {ncol(ll_share)} variables, linelist valid to {fr_date(date_valid)}"
)

#! Export: Donnees/partage is the de-identified copy, never Donnees/propre
export_clean(
  ll_share,
  "linelist-share",
  time_write,
  dir = butembo_share_data_path
)
