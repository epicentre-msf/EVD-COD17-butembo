# Build the shareable vaccinated-case linelist (vax_ll) and compare it with the matched file.
# Kitatumba ETC vaccinated cases, plus pre-ETC cases from the surveillance linelist.

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))

#* Path ------------------------------------
time_write <- time_stamp()
latest_narr_ll_clean <- fs::dir_ls(local_ll_dir, regex = "BUT-EVD") |> max()
etc_ll <- fs::dir_ls(etc_export_nominatif_dir) |> max()

#* Import data -----------------------------
#* surveillance data
ll_clean <- readRDS(latest_narr_ll_clean)

# kitatumba id can sit in id_msf or id_msf_2
ll_clean <- ll_clean |>
  mutate(
    kit_id = case_when(
      str_detect(id_msf, "^CTE-KIT-") ~ id_msf,
      str_detect(id_msf_2, "^CTE-KIT-") ~ id_msf_2,
      .default = NA_character_
    )
  )

#* Kitatumba ETC linelist
kit_ll <- rpxl::rp_xlsb(etc_ll_path_kit, password = "ebolaExport", sheet = 1) |>
  as_tibble() |>
  mutate(phone_number = str_remove(phone_number, "^0"))

#* UCG ETC linelist
ucg_ll <- rpxl::rp_xlsb(etc_ll_path_ucg, password = "ebolaExport", sheet = 1) |>
  as_tibble() |>
  mutate(phone_number = str_remove(phone_number, "^0"))


#* 1. Vaccinated cases in Kitatumba ---------
vax <- kit_ll |>
  filter(str_detect(vaccination_rvsv_yn, "Oui")) |>
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

#* 2. Vaccinated cases did not go to the ETC
# no kit_id: they were never admitted to Kitatumba
extra_vax <- ll_clean |>
  filter(
    str_detect(vaccination_rvsv_yn, "Oui"),
    is.na(kit_id)
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

vax_ll <- bind_rows(vax, extra_vax)
cli::cli_inform(
  "vax_ll: {nrow(vax)} ETC + {nrow(extra_vax)} pre-ETC = {nrow(vax_ll)}"
)

#* 3. Add surveillance fields ---------------
# earliest positive test per pid, so it serves ETC and pre-ETC cases
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

# lab fields stay NA where a pid has no positive test
ll_lookup <- ll_clean |>
  filter(!is.na(kit_id)) |>
  select(kit_id, dhis2_id = pid) |>
  left_join(
    first_positive,
    by = join_by(dhis2_id),
    relationship = "one-to-one"
  )

n_before <- nrow(vax_ll)
vax_ll <- vax_ll |>
  left_join(
    ll_lookup,
    by = join_by(id_msf == kit_id),
    relationship = "many-to-one"
  )
cli::cli_inform(
  "vax_ll: {n_before} row{?s} before ll_clean join, {nrow(vax_ll)} after"
)

# the two pre-ETC cases have no kit_id to join on
manual_dhis2_id <- tibble::tribble(
  ~nom                   , ~dhis2_id             , ~lab_result , ~lab_id             , ~date_lab_sample      ,
  "KAVIRA TSONGO RACHEL" , "RDC-NKV-BUT-26-0209" , "Positif"   , "FHV-NK-BTB-26-234" , as.Date("2026-06-01") ,
  "SEHA KABILA DANIEL"   , "RDC-NKV-BUT-26-0208" , "Positif"   , "FHV-NK-BTB-26-232" , as.Date("2026-06-01")
)

vax_ll <- vax_ll |>
  rows_update(manual_dhis2_id, by = "nom", unmatched = "ignore")

# EVD_status is English upstream; this shared linelist stays French
vax_ll <- vax_ll |>
  mutate(
    EVD_status = case_match(
      EVD_status,
      "Confirmed" ~ "Confirmé",
      .default = EVD_status
    )
  )

n_confirmed <- sum(vax_ll$EVD_status == "Confirmé", na.rm = TRUE)
cli::cli_inform(c(
  "{sum(vax_ll$EVD_status == 'Confirmé' & !is.na(vax_ll$dhis2_id), na.rm = TRUE)} of {n_confirmed} confirmed case{?s} matched a dhis2_id",
  "{sum(vax_ll$EVD_status == 'Confirmé' & !is.na(vax_ll$lab_id), na.rm = TRUE)} of {n_confirmed} confirmed case{?s} matched a lab_id"
))

# confirmed cases still missing a lab_id
vax_ll |> filter(is.na(lab_id), EVD_status == "Confirmé")

#* 4. Compare with the matched version ------
matched_ll <- rio::import(
  fs::path(butembo_matched_ll_dir, "BUT_vaccinated_check_2026-09-28_CR.xlsx")
) |>
  as_tibble()

# identity fields the matching relied on
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

# name fallback for the pre-ETC cases, which have no id_msf; normalised
# so accents and case do not split the same patient
norm_key <- \(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_squish() |>
    str_to_upper()
}
add_key <- \(d) {
  d |>
    mutate(
      match_key = if_else(
        !is.na(id_msf),
        norm_key(as.character(id_msf)),
        norm_key(nom)
      )
    )
}

matched_ll <- add_key(matched_ll)
vax_ll <- add_key(vax_ll)

stopifnot(
  "matched_ll has duplicate match_key" = !anyDuplicated(matched_ll$match_key),
  "vax_ll has duplicate match_key" = !anyDuplicated(vax_ll$match_key)
)

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

# patients in both versions; NA -> value and value -> NA count as changes
vax_changed <- bind_rows(
  to_long(matched_ll, "old"),
  to_long(vax_ll, "new")
) |>
  pivot_wider(names_from = version, values_from = value) |>
  filter(
    match_key %in% matched_ll$match_key,
    match_key %in% vax_ll$match_key
  ) |>
  filter(is.na(old) != is.na(new) | (!is.na(old) & old != new)) |>
  mutate(
    change_type = case_when(
      is.na(old) ~ "filled",
      is.na(new) ~ "lost",
      .default = "changed"
    )
  ) |>
  arrange(match_key, variable)

vax_new_patients <- vax_ll |>
  filter(!match_key %in% matched_ll$match_key)

cli::cli_inform(c(
  "{nrow(vax_changed)} characteristic change{?s} on matched patient{?s}",
  "{nrow(vax_new_patients)} new patient{?s} to send for matching"
))

changed_vars_by_key <- vax_changed |>
  summarise(changed_vars = str_c(variable, collapse = ", "), .by = match_key)

n_before <- nrow(vax_ll)
vax_ll <- vax_ll |>
  left_join(
    changed_vars_by_key,
    by = join_by(match_key),
    relationship = "one-to-one"
  )
cli::cli_inform(
  "vax_ll: {n_before} row{?s} before join, {nrow(vax_ll)} after"
)

vax_ll <- vax_ll |>
  mutate(
    send_for_matching = !match_key %in% matched_ll$match_key,
    values_changed = !is.na(changed_vars),
    .before = changed_vars
  )

#* Export ------------------------------------
vax_ll <- vax_ll |> select(-match_key)

export_clean(
  vax_ll,
  CONFIG$export_prefix,
  "vaccinated-linelist-share",
  time_write,
  dir = butembo_share_data_path
)

rio::export(
  vax_ll,
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
    ),
    by = join_by(dhis2_id),
    relationship = "many-to-one"
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
kit_confirmed <- kit_ll |>
  filter(str_detect(EVD_status, "Confirm"))

kit_confirmed_missing <- kit_confirmed |>
  filter(isolation_site_id == "CTE Kitatumba | Kyangike | Butembo") |>
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

cli::cli_inform(c(
  "{sum(kit_confirmed$isolation_site_id == 'CTE Kitatumba | Kyangike | Butembo', na.rm = TRUE)} of {nrow(kit_confirmed)} confirmed kit_ll case{?s} at the Kitatumba site",
  "{nrow(kit_confirmed_missing)} of those with no kit_id in ll_clean"
))
print(kit_confirmed_missing, n = Inf)
