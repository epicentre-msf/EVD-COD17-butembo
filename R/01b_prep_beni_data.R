# Import, explore and clean Beni data
source(here::here("R", "0_global.R"))

time_write <- time_stamp()

# Import data from tranmission directory
ll_beni_raw <- rio::import(
  fs::path(
    transmission_dir,
    "ZS Beni_MVE_Liste lineaire_20 09 2026_deidentified.xlsx"
  ),
  which = "Base",
  skip = 2
) |>
  as_tibble() |>
  clean_names() |>
  remove_empty()

#* Clean and standardise the LL ------------------------------

ll_beni_clean <- ll_beni_raw |>
  #! keep only the confirmed cases - 285 confirmed cases
  filter(epi_class == "Confirmé") |>

  rename(
    date_symptom_onset = date_ds,
    date_exit_eff = date_statut_final,
    date_admission_eff = date_cte_adm,
    date_notification = date_notif,
    pid = id
  ) |>

  transmute(
    pid,
    # squish first, so a whitespace-only cell counts as empty
    across(where(is.character), ~ na_if(str_squish(.x), "")),
    across(contains("date_"), harmonize_dates),
    # every yes/no field in the export is French. Recoded here, before any
    # comparison below reads one, so nothing downstream tests "Oui"
    across(
      where(is.character),
      \(x) case_match(x, "Oui" ~ "Yes", "Non" ~ "No", .default = x)
    ),
    # French in the raw export, English from here on
    EVD_status = case_match(
      epi_class,
      "Confirmé" ~ "Confirmed",
      .default = epi_class
    ),
    type_of_exit = forcats::fct_recode(
      statut_final,
      Recovered = "Gueri",
      Abandoned = "Abandon",
      Died = "Décédé"
    ),
    type_of_exit = forcats::fct_relevel(
      type_of_exit,
      "Recovered",
      "Abandoned",
      "Died"
    ),
    # Abandoned and an unfilled exit are unsettled, not survivors
    outcome = case_match(
      as.character(type_of_exit),
      "Died" ~ "Died",
      "Recovered" ~ "Recovered",
      .default = "Missing"
    ),
    outcome = factor(outcome, levels = c("Missing", "Recovered", "Died")),
    # onset -> notification (all cases), admission, death or cure (split by
    # outcome). type_of_exit is English by this point, unlike the raw export
    delay_ons_not = as.numeric(date_notification - date_symptom_onset),
    delay_adm = as.numeric(date_admission_eff - date_symptom_onset),
    delay_ons_death = if_else(
      type_of_exit == "Died",
      as.numeric(date_exit_eff - date_symptom_onset),
      NA_real_
    ),
    delay_ons_cure = if_else(
      type_of_exit == "Recovered",
      as.numeric(date_exit_eff - date_symptom_onset),
      NA_real_
    ),
    # unfilled and inconclusive both read as Unknown
    infection_origin = case_when(
      exposition == "Local" ~ "Local",
      exposition == "Importé" ~ "Imported",
      .default = "Unknown"
    ),
    infection_origin = factor(
      infection_origin,
      levels = c("Unknown", "Imported", "Local")
    ),
    sex = case_match(sexe, c("H", "M") ~ "Male", "F" ~ "Female"),
    sex = factor(sex, levels = c("Male", "Female")),
    hcw = if_else(
      is.na(profession),
      NA,
      grepl("Personnel de sant", profession)
    ),

    age = age_years(age, age_unite),
    age_group = cut(
      age,
      breaks = CONFIG$age_breaks,
      right = FALSE,
      labels = CONFIG$age_labs
    ),
    #! Place of residence / onset
    adm1_name__onset = case_when(
      !is.na(visiteur_res) ~ NA,
      .default = province
    ),
    adm2_name__onset = case_when(
      !is.na(visiteur_res) ~ NA,
      .default = zs
    ),
    adm3_name__onset = case_when(
      !is.na(visiteur_res) ~ NA,
      .default = as
    ),
    dead_upon_notif = case_when(
      status_initial == "Décédé" ~ TRUE,
      status_initial == "Vivant" ~ FALSE
    ),
    # ! Isolated in an ETC ?
    isolated_etc = case_when(
      status_initial == "Vivant" & cte_adm == "Oui" ~ TRUE,
      status_initial == "Vivant" & cte_adm == "Non" ~ FALSE,
      .default = NA
    ),
    # ! Which ETC ?
    etc_site = case_when(
      isolated_etc ~ cte
    )
  )

#* Duplicate and missing pid ---------------
# ! no duplicated IDs
dupes_id <- ll_beni_clean |>
  filter(!is.na(pid)) |>
  janitor::get_dupes(pid)

cli::cli_alert_warning(
  "{nrow(dupes_id)} duplicated rows across {n_distinct(dupes_id$pid)} pid"
)

cli::cli_alert_warning("{sum(is.na(ll_beni_clean$pid))} rows with no pid")

#! Export the linelist: Donnees/propre on local only
export_clean(
  ll_beni_clean,
  "BEN-EVD",
  "linelist",
  time_write,
  dir = local_ll_dir
)
