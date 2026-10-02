# Build the shareable vaccinated-case linelist (vax_ll) and compare it with the matched file.
# Kitatumba ETC vaccinated cases, plus pre-ETC cases from the surveillance linelist.

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))

#* Path ------------------------------------
time_write <- time_stamp()
latest_narr_ll_clean <- fs::dir_ls(local_ll_dir, regex = "BUT-EVD") |> max()

#* Import data -----------------------------
#* 1. Vaccinated cases in MSF ETC or CT (Kitatumba, UCG) ---------

#* Kitatumba ETC linelist
kit_ll <- rpxl::rp_xlsb(etc_ll_path_kit, password = "ebolaExport", sheet = 1) |>
  as_tibble()

kit_sub <- kit_ll |>
  transmute(
    isolation_site_id,
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

#* UCG ETC linelist - remove cases transferred to Kitatumba ETC
ucg_ll <- rpxl::rp_xlsb(etc_ll_path_ucg, password = "ebolaExport", sheet = 1) |>
  as_tibble() |>
  filter(type_of_exit != "Transféré au CTE")

ucg_sub <- ucg_ll |>
  transmute(
    isolation_site_id,
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

# Bind together vaccinated from ETCs
vax_etc <- bind_rows(kit_sub, ucg_sub) |>
  # keep vax
  filter(str_detect(vaccination_rvsv_yn, "Oui"))

#* 2. Vaccinated cases did not go to the ETC (surveillance linelist)
ll_clean <- readRDS(latest_narr_ll_clean)

ll_clean <- ll_clean |>
  mutate(
    # kitatumba id can sit in id_msf or id_msf_2
    has_msf_id = if_any(c(id_msf, id_msf_2), ~ !is.na(.x)),
    # prep recoded these to English; ETC exports are French
    EVD_status = case_match(
      EVD_status,
      "Confirmed" ~ "Confirmé",
      .default = EVD_status
    ),
    sex = case_match(as.character(sex), "Male" ~ "M", "Female" ~ "F"),
    age = age_raw,
    age_unit = case_match(
      age_unit,
      "Years" ~ "Ans",
      "Months" ~ "Mois",
      "Days" ~ "Jour",
      .default = age_unit
    ),
    vaccination_rvsv_yn = str_replace(vaccination_rvsv_yn, "Yes", "Oui")
  )

extra_vax <- ll_clean |>
  # vax with no msf_id
  filter(
    str_detect(vaccination_rvsv_yn, "Oui"),
    !has_msf_id
  ) |>
  transmute(
    id_msf,
    EVD_status,
    nom,
    sex,
    age = as.numeric(age),
    age_unit,
    job,
    adm1_name__res,
    adm2_name__res,
    adm3_name__res,
    type_of_exit,
    vaccination_rvsv_yn,
    year_vaccination_rvsv
  )

#* Combine all vaccinated ---------------------------------------------
vax_ll <- bind_rows(vax_etc, extra_vax)
cli::cli_inform(
  "vax_ll: {nrow(vax_etc)} ETC + {nrow(extra_vax)} pre-ETC = {nrow(vax_ll)}"
)

#* Add surveillance fields ---------------
#* add the pid, and first positive lab ID to check on match
#* using the surveillance linelist

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
#* One row per Kitatumba / UCG id, whichever of the two id columns holds it
ll_lookup <- ll_clean |>
  select(id_msf, id_msf_2, dhis2_id = pid) |>
  pivot_longer(c(id_msf, id_msf_2), values_to = "msf_id") |>
  filter(str_detect(msf_id, "^(CTE-KIT-|CT-UCG)"), !is.na(dhis2_id)) |>
  distinct(msf_id, dhis2_id) |>
  left_join(
    first_positive,
    by = join_by(dhis2_id),
    relationship = "many-to-one"
  )

stopifnot(
  "ll_lookup has duplicate msf_id" = !anyDuplicated(ll_lookup$msf_id)
)

n_before <- nrow(vax_ll)
vax_ll <- vax_ll |>
  left_join(
    ll_lookup,
    by = join_by(id_msf == msf_id),
    relationship = "many-to-one"
  )
cli::cli_inform(
  "vax_ll: {n_before} row{?s} before ll_clean join, {nrow(vax_ll)} after"
)

# cases with no msf id have nothing to join on; we manually match namesto make sure the join works
name_crosswalk <- tibble::tribble(
  ~name_msf              , ~name_surveillance     ,
  "KAVIRA TSONGO RACHEL" , "KAVIRA TSONGO RACHEL" ,
  "SEHA KABILA DANIEL"   , "SEHA KABILA DANIEL"
)

ll_name_lookup <- ll_clean |>
  filter(!is.na(pid), nom %in% name_crosswalk$name_surveillance) |>
  distinct(name_surveillance = nom, dhis2_id = pid) |>
  left_join(
    first_positive,
    by = join_by(dhis2_id),
    relationship = "many-to-one"
  )

stopifnot(
  "a name_surveillance matches several pids" = !anyDuplicated(
    ll_name_lookup$name_surveillance
  )
)

manual_ids <- name_crosswalk |>
  left_join(
    ll_name_lookup,
    by = join_by(name_surveillance),
    relationship = "one-to-one"
  ) |>
  select(nom = name_msf, dhis2_id, lab_result, lab_id, date_lab_sample)

cli::cli_inform(
  "{sum(!is.na(manual_ids$dhis2_id))} of {nrow(manual_ids)} crosswalk name{?s} found in ll_clean"
)

# unresolved names are skipped, not blanked
vax_ll <- vax_ll |>
  rows_update(
    filter(manual_ids, !is.na(dhis2_id)),
    by = "nom",
    unmatched = "error"
  )

n_confirmed <- sum(vax_ll$EVD_status == "Confirmé", na.rm = TRUE)
cli::cli_inform(c(
  "{sum(vax_ll$EVD_status == 'Confirmé' & !is.na(vax_ll$dhis2_id), na.rm = TRUE)} of {n_confirmed} confirmed case{?s} matched a dhis2_id",
  "{sum(vax_ll$EVD_status == 'Confirmé' & !is.na(vax_ll$lab_id), na.rm = TRUE)} of {n_confirmed} confirmed case{?s} matched a lab_id"
))

#* Compare with matched version ------------------------------------------------------------------
time_write_hm <- format(Sys.time(), "%Y%m%d_%H%M")

matched_files <- fs::dir_ls(
  check_match_dir,
  regex = "vaccinated_check_\\d{4}-\\d{2}-\\d{2}.*\\.xlsx$"
)
stopifnot("no vaccinated_check file in check-match" = length(matched_files) > 0)

file_dates <- fs::path_file(matched_files) |>
  str_extract("\\d{4}-\\d{2}-\\d{2}") |>
  as.Date()
matched_files <- matched_files[order(file_dates)]

matched_all <- matched_files |>
  rlang::set_names() |>
  purrr::map(\(f) {
    rio::import(f) |>
      as_tibble() |>
      mutate(across(everything(), as.character))
  }) |>
  bind_rows(.id = "source_file") |>
  mutate(
    # excel drops the leading 0 of 10-digit numbers, leaving 9 digits
    phone_number = if_else(
      str_detect(phone_number, "^[1-9]\\d{8}$"),
      str_c("0", phone_number),
      phone_number
    )
  )
cli::cli_inform(
  "{length(matched_files)} check-match file{?s}, {nrow(matched_all)} row{?s}"
)

# identity fields the matching relied on
match_vars <- c(
  "phone_number",
  "sex",
  "age",
  "age_unit",
  "job",
  "adm1_name__res",
  "adm2_name__res",
  "adm3_name__res"
)

# name fallback keys for the pre-ETC cases, which have no id_msf
matched_all <- add_key(matched_all)
vax_ll <- add_key(vax_ll)

# files are ordered oldest first, so the last row per patient is the latest match
matched_ll <- matched_all |>
  slice_tail(n = 1, by = match_key)
cli::cli_inform(
  "{nrow(matched_all) - nrow(matched_ll)} older matched row{?s} superseded by a later file"
)

stopifnot(
  "vax_ll has duplicate match_key" = !anyDuplicated(vax_ll$match_key)
)

# patients in both versions; NA -> value counts as a change, value -> NA does not
vax_changed <- bind_rows(
  to_long(matched_ll, "old", match_vars),
  to_long(vax_ll, "new", match_vars)
) |>
  pivot_wider(names_from = version, values_from = value) |>
  filter(
    match_key %in% matched_ll$match_key,
    match_key %in% vax_ll$match_key
  ) |>
  # value -> NA is ignored: a blank in the new export is not new information
  filter(
    (is.na(old) & !is.na(new)) | (!is.na(old) & !is.na(new) & old != new)
  ) |>
  mutate(change_type = if_else(is.na(old), "filled", "changed")) |>
  arrange(match_key, variable)

vax_new_patients <- vax_ll |>
  filter(!match_key %in% matched_ll$match_key)

cli::cli_inform(c(
  "{nrow(vax_changed)} characteristic change{?s} on matched patient{?s}",
  "{nrow(vax_new_patients)} new patient{?s} to send for matching"
))

changed_vars_by_key <- vax_changed |>
  summarise(changed_vars = str_c(variable, collapse = ", "), .by = match_key)

vax_ll <- vax_ll |>
  left_join(
    changed_vars_by_key,
    by = join_by(match_key),
    relationship = "one-to-one"
  )

vax_ll <- vax_ll |>
  mutate(
    send_for_matching = !match_key %in% matched_ll$match_key,
    values_changed = !is.na(changed_vars),
    .before = changed_vars
  )

# new vaccinated cases, or matched ones whose identity fields moved
to_rematch <- vax_ll |>
  filter(send_for_matching | values_changed)
cli::cli_inform(
  "{nrow(to_rematch)} to rematch: {sum(to_rematch$send_for_matching)} new, {sum(to_rematch$values_changed)} changed"
)

match_status_vars <- c("match_ervebo_db", "match_detail")
stopifnot(
  "match status column missing from matched files" = all(
    match_status_vars %in% names(matched_ll)
  )
)

# only unchanged, already-matched patients keep their previous match status
match_status <- matched_ll |>
  select(match_key, all_of(match_status_vars)) |>
  filter(
    match_key %in% filter(vax_ll, !send_for_matching, !values_changed)$match_key
  )

n_before <- nrow(vax_ll)
vax_ll <- vax_ll |>
  left_join(
    match_status,
    by = join_by(match_key),
    relationship = "one-to-one"
  )
cli::cli_inform(
  "vax_ll: {n_before} row{?s} before match status join, {nrow(vax_ll)} after"
)

#* Export ------------------------------------
vax_ll <- vax_ll |> select(-match_key)
to_rematch <- to_rematch |>
  select(-c(match_key, EVD_status, type_of_exit, lab_result, date_lab_sample))

export_clean(
  vax_ll,
  CONFIG$export_prefix,
  "vaccinated-linelist-share",
  time_write,
  dir = butembo_share_data_path
)

fs::dir_create(c(vax_ll_out_dir, to_match_dir))

qxl::qxl(
  vax_ll,
  file = fs::path(
    vax_ll_out_dir,
    glue::glue(
      "{CONFIG$export_prefix}_vaccinated-linelist__{time_write_hm}.xlsx"
    )
  ),
  filter = TRUE
)

if (nrow(to_rematch) > 0) {
  qxl::qxl(
    to_rematch,
    file = fs::path(
      to_match_dir,
      glue::glue("{CONFIG$export_prefix}_to-be-matched__{time_write_hm}.xlsx")
    ),
    filter = TRUE
  )
} else {
  cli::cli_inform("0 patients to send for matching, no file saved")
}

#* DHIS2 IDS ------------------------------------------------------------------------------------

# Full list of confirmed cases that have been in Kitatumba
confirmed_kit <- ll_clean |>
  filter(has_msf_id, !is.na(pid)) |>
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
