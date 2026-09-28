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

#* CONFIRMED CASES ---------------------------------------------

# find all vaccinated cases from ETC
etc_ll <- fs::dir_ls(fs::path(
  butembo_project_data_path,
  "linelists",
  "cte_kitatumba",
  "export_nominatif"
)) |>
  max()

# all vaccinated from ETC linelist, remove teh confirmed (in surveillance)
kit_ll <- rpxl::rp_xlsb(etc_ll, password = "ebolaExport", sheet = 1) |>
  as_tibble()

# keep vaccinated non-case
vax <- kit_ll |>
  filter(
    str_detect(vaccination_rvsv_yn, "Oui"),
    #str_detect(isolation_site_id, "CTE Kitatumba")
  ) |>
  transmute(
    id_msf = patient_site_id,
    EVD_status,
    nom = patient_name,
    phone_number,
    sex,
    age,
    age_unit,
    job,
    adm1_name__res,
    adm2_name__res,
    adm3_name__res,
    vaccination_rvsv_yn,
    type_of_exit
  )

# find all vaccinated cases from surveillance
# earliest visit to a CT/CTE-named facility, one row per id_msf at most
first_ct_entry <- ll_clean |>
  filter(str_detect(vaccination_rvsv_yn, "Oui")) |>
  select(
    id_msf,
    contains("HF_name_visited"),
    contains("date_start_HF_visited")
  ) |>
  mutate(across(-id_msf, as.character)) |>
  pivot_longer(
    cols = -id_msf,
    names_to = c(".value", "visit"),
    names_pattern = "(.*?)(\\d+)$",
    names_transform = list(visit = as.integer)
  ) |>
  rename_with(\(x) str_remove(x, "_$")) |>
  # word boundary excludes CTE - CT and CTE are distinct facility types here
  filter(str_detect(HF_name_visited, "\\bCT\\b")) |>
  mutate(date_start_HF_visited = as.Date(date_start_HF_visited)) |>
  slice_min(date_start_HF_visited, by = id_msf, n = 1, with_ties = FALSE) |>
  select(id_msf, entry_ct = date_start_HF_visited)

ll_clean <- ll_clean |>
  left_join(first_ct_entry, by = join_by(id_msf), relationship = "one-to-one")

# add exposure variables to the ETC linelist
vax_clean <- vax |>
  left_join(
    select(
      ll_clean,
      id_msf,
      year_vaccination_rvsv,
      entry_ct,
      contact_EVD_case,
      contact_funeral,
      contact_HF,
      contact_tradi,
      contact_travel,
      transmission_type_1
    ),
    by = join_by(id_msf)
  )

# add the two cases before the ETC opened
extra_vax <- ll_clean |>
  filter(
    str_detect(vaccination_rvsv_yn, "Oui"),
    is.na(id_msf)
  ) |>
  transmute(
    id_msf,
    EVD_status,
    nom,
    sex,
    age,
    age_unit,
    job,
    adm1_name__res,
    adm2_name__res,
    adm3_name__res,
    date_symptom_onset,
    date_admission_eff,
    date_exit_eff,
    type_of_exit,
    vaccination_rvsv_yn,
    year_vaccination_rvsv,
    entry_ct,
    contact_EVD_case,
    contact_funeral,
    contact_HF,
    contact_tradi,
    contact_travel,
    transmission_type_1
  )

vax_full <- bind_rows(vax_clean, extra_vax)

export_clean(
  vax_full,
  CONFIG$export_prefix,
  "vaccinated-linelist-share",
  time_write,
  dir = butembo_share_data_path
)
