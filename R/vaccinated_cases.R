# Build the shareable vaccinated-case linelist from the ETC export.
# Joins surveillance exposure variables, adds the two pre-ETC cases.

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))

#* Path ------------------------------------
time_write <- time_stamp()
latest_narr_ll_clean <- fs::dir_ls(local_ll_dir, regex = "BUT-EVD") |> max()

#* Import data -----------------------------
# latest surveillance linelist
ll_clean <- readRDS(latest_narr_ll_clean)

# kitatumba id can be recorded under id_msf or id_msf_2 - resolve to one column
ll_clean <- ll_clean |>
  mutate(
    kit_id = case_when(
      str_detect(id_msf, "^CTE-KIT-") ~ id_msf,
      str_detect(id_msf_2, "^CTE-KIT-") ~ id_msf_2,
      .default = NA_character_
    )
  )

#* Vaccinated cases -------------------------

etc_ll <- fs::dir_ls(fs::path(
  butembo_project_data_path,
  "linelists",
  "cte_kitatumba",
  "export_nominatif"
)) |>
  max()

# kitatumba linelists
kit_ll <- rpxl::rp_xlsb(etc_ll, password = "ebolaExport", sheet = 1) |>
  as_tibble() |>
  mutate(phone_number = str_remove(phone_number, "^0"))

# All vaccinated cases who have been in Kitatumba
vax <- kit_ll |>
  filter(
    str_detect(vaccination_rvsv_yn, "Oui")
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

# earliest positive lab test per pid, so it serves ETC and pre-ETC cases
first_positive <- ll_clean |>
  filter(!is.na(pid)) |>
  select(
    pid,
    contains("date_lab_sample_"),
    contains("lab_result_"),
    contains("lab_id_")
  ) |>
  mutate(across(-pid, as.character)) |>
  pivot_longer(
    cols = -pid,
    names_to = c(".value", "test"),
    names_pattern = "(.*?)(\\d+)$",
    names_transform = list(test = as.integer)
  ) |>
  rename_with(\(x) str_remove(x, "_$")) |>
  filter(lab_result == "Positif") |>
  mutate(date_lab_sample = as.Date(date_lab_sample)) |>
  slice_min(date_lab_sample, by = pid, n = 1, with_ties = FALSE) |>
  select(dhis2_id = pid, lab_id, lab_result, date_lab_sample)

# pid for every matched case, lab fields only where a positive exists
ll_lookup <- ll_clean |>
  filter(!is.na(kit_id)) |>
  select(kit_id, dhis2_id = pid) |>
  left_join(
    first_positive,
    by = join_by(dhis2_id),
    relationship = "one-to-one"
  )

n_before <- nrow(vax)
vax_clean <- vax |>
  left_join(
    ll_lookup,
    by = join_by(id_msf == kit_id),
    relationship = "many-to-one"
  )
cli::cli_inform(
  "vax_clean: {n_before} row{?s} before ll_clean join, {nrow(vax_clean)} after"
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
    type_of_exit,
    vaccination_rvsv_yn,
    year_vaccination_rvsv
  )

#* Manual DHIS2 / lab ids for the pre-ETC cases ----
manual_dhis2_id <- tibble::tribble(
  ~nom                   , ~dhis2_id             , ~lab_result , ~lab_id             , ~date_lab_sample      ,
  "KAVIRA TSONGO RACHEL" , "RDC-NKV-BUT-26-0209" , "Positif"   , "FHV-NK-BTB-26-234" , as.Date("2026-06-01") ,
  "SEHA KABILA DANIEL"   , "RDC-NKV-BUT-26-0208" , "Positif"   , "FHV-NK-BTB-26-232" , as.Date("2026-06-01")
)

vax_full <- bind_rows(vax_clean, extra_vax)

# EVD_status is recoded to English upstream - this shared linelist stays French
vax_full <- vax_full |>
  mutate(
    EVD_status = case_match(
      EVD_status,
      "Confirmed" ~ "Confirmé",
      .default = EVD_status
    )
  )


vax_full <- vax_full |>
  rows_update(manual_dhis2_id, by = "nom", unmatched = "ignore")

# 1 / 9 confirmed has no DHIS2 Id in the ensemble -- not in BUT LL
n_confirmed <- sum(vax_full$EVD_status == "Confirmé")
cli::cli_inform(
  "{sum(vax_full$EVD_status == 'Confirmé' & !is.na(vax_full$dhis2_id))} of {n_confirmed} confirmed case{?s} matched a dhis2_id"
)

cli::cli_inform(
  "{sum(vax_full$EVD_status == 'Confirmé' & !is.na(vax_full$lab_id))} of {n_confirmed} confirmed case{?s} matched a lab_id"
)

# one case has no pid and no lab_id
vax_full |> filter(is.na(lab_id), EVD_status == "Confirmé")

#* Compare with matched version -------------
butembo_matched_ll_dir <- fs::path(
  onedrive,
  "Ebola Outbreaks - COD_UGA-2026",
  "COD",
  "data-raw",
  "Butembo-surv",
  "data"
)

matched_ll <- rio::import(
  fs::path(butembo_matched_ll_dir, "BUT_vaccinated_check_2026-09-28_CR.xlsx")
) |>
  as_tibble()

# identity fields matching relies on - adjust if matched_ll names these differently
match_vars <- c(
  "nom",
  "phone_number",
  "sex",
  "age",
  "age_unit",
  "job",
  "adm1_name__res",
  "adm2_name__res",
  "adm3_name__res"
)

# fall back to matching by name where id_msf is missing (the two pre-ETC
# cases predate the ETC export and were never assigned one)
matched_ll <- matched_ll |>
  mutate(match_key = if_else(!is.na(id_msf), as.character(id_msf), nom))

vax_full <- vax_full |>
  mutate(match_key = if_else(!is.na(id_msf), as.character(id_msf), nom))

to_long <- \(d, version) {
  d |>
    select(all_of(c("match_key", match_vars))) |>
    mutate(across(all_of(match_vars), as.character)) |>
    pivot_longer(
      all_of(match_vars),
      names_to = "variable",
      values_to = "value"
    ) |>
    mutate(version = version)
}

stopifnot(
  "matched_ll has duplicate match_key" = !anyDuplicated(matched_ll$match_key),
  "vax_full has duplicate match_key" = !anyDuplicated(vax_full$match_key)
)

# characteristics changed on patients present in both versions
vax_changed <- bind_rows(
  to_long(matched_ll, "old"),
  to_long(vax_full, "new")
) |>
  pivot_wider(names_from = version, values_from = value) |>
  filter(!is.na(old), !is.na(new), old != new) |>
  arrange(match_key, variable)

# patients in the new version that were never in the matched one
vax_new_patients <- vax_full |>
  filter(!match_key %in% matched_ll$match_key)

cli::cli_inform(c(
  "{nrow(vax_changed)} characteristic change{?s} on matched patient{?s}",
  "{nrow(vax_new_patients)} new patient{?s} to send for matching"
))

# per-case review flags ----------------------
changed_vars_by_key <- vax_changed |>
  summarise(changed_vars = str_c(variable, collapse = ", "), .by = match_key)

n_before <- nrow(vax_full)
vax_full <- vax_full |>
  left_join(
    changed_vars_by_key,
    by = join_by(match_key),
    relationship = "one-to-one"
  )
cli::cli_inform(
  "vax_full: {n_before} row{?s} before join, {nrow(vax_full)} after"
)

vax_full <- vax_full |>
  mutate(
    send_for_matching = !match_key %in% matched_ll$match_key,
    values_changed = !is.na(changed_vars),
    .before = changed_vars
  )


#* Export ------------------------------------
vax_full <- vax_full |> select(-match_key)

export_clean(
  vax_full,
  CONFIG$export_prefix,
  "vaccinated-linelist-share",
  time_write,
  dir = butembo_share_data_path
)

rio::export(
  vax_full,
  fs::path(
    butembo_matched_ll_dir,
    glue::glue("{CONFIG$export_prefix}_vaccinated-linelist__{time_write}.xlsx")
  )
)

# Full list of confirmed cases that have been in Kitatumba
confirmed_kit <- ll_clean |>
  filter(!is.na(kit_id), !is.na(pid)) |>
  select(kit_id, dhis2_id = pid) |>
  left_join(
    select(
      first_positive,
      dhis2_id,
      first_pos_lab_id = lab_id,
      first_pos_date_lab_sample = date_lab_sample
    )
  ) |>
  print(n = Inf)

# every confirmed kit_id should exist in the ETC export
kit_unmatched <- confirmed_kit |>
  anti_join(kit_ll, by = join_by(kit_id == patient_site_id))

cli::cli_inform(
  "{nrow(confirmed_kit) - nrow(kit_unmatched)} of {nrow(confirmed_kit)} confirmed_kit id{?s} found in kit_ll"
)

rio::export(
  confirmed_kit,
  fs::path(
    butembo_matched_ll_dir,
    glue::glue(
      "{CONFIG$export_prefix}_list-dhis2-id-kitatumba-confirmed__{time_write}.xlsx"
    )
  )
)

#* Exploratory: confirmed in kit_ll but no kit_id in ll_clean ----
kit_confirmed_missing <- kit_ll |>
  filter(str_detect(EVD_status, "Confirm")) |>
  anti_join(
    filter(ll_clean, !is.na(kit_id)),
    by = join_by(patient_site_id == kit_id)
  ) |>
  select(
    patient_site_id,
    nom = patient_name,
    phone_number,
    sex,
    age,
    age_unit,
    adm3_name__res
  )

cli::cli_inform(
  "{nrow(kit_confirmed_missing)} confirmed kit_ll case{?s} with no kit_id in ll_clean"
)
print(kit_confirmed_missing, n = Inf)
