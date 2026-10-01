clean_msf_linelist <- function(fn, root = here::here('Donnees')) {
  out <- rio::import(
      here::here(root, fn)
    ) |>
    remove_empty() |>
    select(
      # status
      status = EVD_status,
      doa = dead_upon_arrival,
      # lab result
      starts_with('lab_result'),
      starts_with('date_lab_result'),
      # demography
      age, age_unit, sex,
      # outcome
      outcome = type_of_exit,
      active = ACTIF,
      # dates
      date_in = date_admission_eff,
      date_out = date_exit_eff,
      date_sym = date_symptom_onset,
      date_notif = date_notification,
    ) |>
    mutate(
      # date helpers
      date_death = case_when(outcome == 'Décédé' ~ date_out),
      across(starts_with('date'), as.Date),
      week = floor_date(date_in,
                        'week',
                        week_start = 1),
      # fix age
      age = case_when(
        age_unit == 'Mois' ~ as.numeric(age) / 12,
        age_unit == 'Jour' ~ as.numeric(age) / 365,
        .default = as.numeric(age)),
      date_conf = find_result_date(pick(everything())),
      # simplified outcomes
      #outcome = case_when(grepl(outcome, 'non cas') ~ 'Non-Cas', 
                          #.default = outcome),
      # delays
      delay_notif = date_notif - date_sym,
      delay_adm = date_in - date_sym,
      delay_out = date_out - date_sym,
      delay_death = case_when(outcome == 'Décédé' ~ delay_out),
      delay_recov = case_when(outcome == 'Guéri' ~ delay_out),
      # als
      stay = date_out - date_in,
      stay_conf = case_when(
        is.na(date_in) ~ date_out - date_conf,
        is.na(date_conf) ~ date_out - date_in,
        date_conf <= date_in ~ date_out - date_in,
        .default = date_out - date_conf
      ),
      stay_conf = case_when(stay_conf < 0 ~ NA,
                            #stay_conf == 0 ~ 0.5,
                            .default = as.numeric(stay_conf)),
      stay_sus = case_when(status == 'Confirmé' ~ date_conf - date_in,
                           .default = date_out - date_in),
      stay_sus = case_when(stay_sus <= 0 ~ NA,
                           #stay_sus == 0 ~ 0.5,
                           .default = as.numeric(stay_sus))
    )

  return(out)
}
