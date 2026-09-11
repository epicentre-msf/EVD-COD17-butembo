clean_ensemble_linelist <- function(fn, root = here::here('Donnees'),
                                    out_root = here::here('Donnees', 'propre'), save = TRUE) {
  out <- rio::import(
      here::here(root, fn),
      which = 'data',
      skip = 2
    ) |>
    remove_empty(which = 'rows') |>
    select(
      # id
      id = patient_site_id,
      # dates
      date = date_lab_result_1, date_exit = date_exit_eff, date_onset = date_symptom_onset,
      date_adm = date_admission_eff, date_notif = date_notification,
      date_sample = date_lab_sample_1,
      # places
      adm1 = adm1_name__notif, adm2 = adm2_name__notif, adm3 = adm3_name__notif,
      # demography
      age, age_unit, sex, job,
      # outcomes
      outcome = type_of_exit_simple, 
      outcome_detail = type_of_exit, 
      doa = sample_type_1,
      # hf
      hf_iso = isolation_site_id,
      hf_noti = facility_notification,
      hf_visit_1 = HF_name_visited_1,
      hf_visit_2 = HF_name_visited_2,
      hf_visit_3 = HF_name_visited_3,
      hf_visit_4 = HF_name_visited_4,
      hf_visit_5 = HF_name_visited_5,
      hf_visit_6 = HF_name_visited_6,
      # transmission / surveillance monitor
      inf_type_1 = transmission_type_1, inf_place_1 = transmission_place_1, 
      inf_hf_1 = HF_name_transmission_1, inf_id_1 = infector_ID_1, inf_date_1 = date_transmission_1,
      inf_adm2_1 = adm2_name__transmission_1, inf_adm3_1 = adm3_name__transmission_1,
      inf_type_2 = transmission_type_2, inf_place_2 = transmission_place_2, 
      inf_hf_2 = HF_name_transmission_2, inf_id_2 = infector_ID_2, inf_date_2 = date_transmission_2,
      inf_adm2_2 = adm2_name__transmission_2, inf_adm3_2 = adm3_name__transmission_2,
      narrative = narratif
    ) |>
    mutate(
      across(starts_with('date'), as.Date),
      week = floor_date(date,
                        'week',
                        week_start = 1),
      hcw = ifelse(is.na(job), NA, grepl('Personnel de sant', job)),
      delay_adm = as.numeric(date_adm - date_onset),
      delay_notif = as.numeric(date_notif - date_onset),
      delay_out = as.numeric(date_exit - date_onset),
      age = case_when(
        age_unit == 'Mois' ~ as.numeric(age) / 12,
        age_unit == 'Jour' ~ as.numeric(age) / 365,
        .default = as.numeric(age)),
      doa = doa == 'Swab'
    ) |>
    select(-age_unit)

  # don't name check for empties
  out_nowhere <- out |> 
    filter(
      is.na(adm3) 
    ) |>
    rename(adm1_raw = adm1,
           adm2_raw = adm2,
           adm3_raw = adm3)

  out <- out |>
    filter(
      !(is.na(adm3))
    ) |>
    clean_adm_names(
      by = c('adm1', 'adm2', 'adm3'),
      code_col = 'adm3_pcode',
      require_all = FALSE,
      keep_raw = TRUE,
      fn_ref = here::here('Donnees', 'spatiale', 'adm3_ref.csv'),
      fn_fixes = here::here('Donnees', 'spatiale', 'adm3_fixes.csv')
    ) |>
    bind_rows(out_nowhere) |>
    arrange(week, adm1, adm2, adm3)

  if (save) {
    rio::export(out, here::here(out_root, 'ensemble_clean.rds'))
  }

  return(out)
}
