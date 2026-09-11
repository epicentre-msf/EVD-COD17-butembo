# Clean the linelist, health-facility visits, alert and contact data.
# The only script that reads SharePoint. Exports to Donnees/propre and
# rsyncs app_data.rds to the evd-2026-app dashboard on episerv.

#* TODO ------------------------------------

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))

#* Path ------------------------------------
time_write <- time_stamp()

#* Local geo cache --------------------------
# 0_global.R's adm1/adm2/adm3 may already be the local cache read back, so
# re-read straight from SharePoint here to actually refresh it
adm1 <- readRDS(fs::path(sf_data_path, "COD_adm1_sub.rds"))
adm2 <- readRDS(fs::path(sf_data_path, "COD_adm2_sub.rds"))
adm3 <- readRDS(fs::path(sf_data_path, "COD_adm3_sub.rds"))

saveRDS(adm1, fs::path(local_geobase_dir, "COD_adm1_sub.rds"))
saveRDS(adm2, fs::path(local_geobase_dir, "COD_adm2_sub.rds"))
saveRDS(adm3, fs::path(local_geobase_dir, "COD_adm3_sub.rds"))

#* Import data -----------------------------
ll_narr <- rio::import(latest_narr_ll, sheet = "data", skip = 2) |>
  as_tibble() |>
  # rows only: dropping empty columns would remove a field nobody filled
  janitor::remove_empty(which = "rows")

n_import <- nrow(ll_narr)

#* CLEAN LINELIST --------------------------
ll_narr_clean <- ll_narr |>
  mutate(
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
      EVD_status,
      "Confirmé" ~ "Confirmed",
      .default = EVD_status
    ),
    type_of_exit = forcats::fct_recode(
      type_of_exit,
      Recovered = "Guéri",
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
    infection_butembo = case_when(
      infection_butembo %in% c("Yes", "Musienene") ~ "Local",
      infection_butembo == "No" ~ "Imported",
      .default = "Unknown"
    ),
    infection_butembo = factor(
      infection_butembo,
      levels = c("Unknown", "Imported", "Local")
    ),
    sex = case_match(sex, c("H", "M") ~ "Male", "F" ~ "Female"),
    sex = factor(sex, levels = c("Male", "Female")),
    # grepl(NA) reads FALSE, not NA, so an unfilled job would misread as not hcw
    hcw = if_else(is.na(job), NA, grepl("Personnel de sant", job)),
    # a 9-month-old is age 9 until the unit is applied
    age_raw = age, # the pair age_unit describes, kept for the share export
    age = age_years(age, age_unit),
    # after age_years(), which matches on the French tokens
    age_unit = case_match(
      age_unit,
      "Ans" ~ "Years",
      "Mois" ~ "Months",
      "Jour" ~ "Days",
      .default = age_unit
    ),
    age_group = cut(
      age,
      breaks = CONFIG$age_breaks,
      right = FALSE,
      labels = CONFIG$age_labs
    ),
    # split "site | aire de santé | zone de santé" into 3 columns
    adm3_isolation = str_squish(str_split_i(isolation_site_id, fixed("|"), 2)),
    adm2_isolation = str_squish(str_split_i(isolation_site_id, fixed("|"), -1)),
    isolation_site_id = str_squish(str_split_i(
      isolation_site_id,
      fixed("|"),
      1
    )),

    #! Place of Notification

    adm1_name = case_when(
      res_equal_onset == "Yes" ~ adm1_name__res,
      .default = adm1_name__onset
    ),
    adm2_name = case_when(
      res_equal_onset == "Yes" ~ adm2_name__res,
      .default = adm2_name__onset
    ),

    adm3_name = case_when(
      res_equal_onset == "Yes" ~ adm3_name__res,
      .default = adm3_name__onset
    ),

    # inferred only from a complete admission-exit pair: same day means the
    # patient never spent a night in care, so was dead at notification
    dead_upon_notif = case_when(
      !is.na(dead_upon_arrival) ~ dead_upon_arrival == "Yes",
      outcome == "Recovered" ~ FALSE,
      # still admitted, or no dates at all: nothing to infer from
      is.na(date_admission_eff) | is.na(date_exit_eff) ~ NA,
      date_exit_eff == date_admission_eff ~ TRUE,
      .default = FALSE
    ),

    # ! Isolated in an ETC ?
    # both sites took ordinary patients before these dates
    isolated_etc = case_when(
      isolation_site_id == "HGR Katwa" &
        date_admission_eff >= as.Date("2026-06-15") &
        # unknown counts as alive at notification, so the cohort keeps them
        !dead_upon_notif %in% TRUE ~ TRUE,
      # an MSF id also marks a Kitatumba admission, whatever the date
      isolation_site_id %in%
        c("HGR Kitatumba", "CTE Kitatumba") &
        (date_admission_eff >= as.Date("2026-07-01") | !is.na(id_msf)) &
        !dead_upon_notif %in% TRUE ~ TRUE,
      .default = FALSE
    ),

    # ! Which ETC ?
    etc_site = case_when(
      isolated_etc & isolation_site_id == "HGR Katwa" ~ "CTE Katwa (MEDAIR)",
      isolated_etc &
        isolation_site_id %in% c("HGR Kitatumba", "CTE Kitatumba") ~
        "CTE Kitatumba (MSF)"
    ),

    # ! Where was death ?
    # the field mixes a place with a legacy yes/no: "Yes" is taken at its
    # word, "No" only says it was not community, so the ETC record decides
    death_place = case_when(
      outcome != "Died" ~ NA_character_,
      community_death %in% c("CTE/CT", "CTE", "CT") ~ "CTE/CT",
      community_death %in% c("ESS", "Communauté") ~ "Community",
      community_death == "Yes" ~ "Community",
      community_death == "No" & isolated_etc ~ "CTE/CT",
      community_death == "No" ~ "Community",
      .default = NA_character_
    ),
    death_place = factor(death_place, levels = c("CTE/CT", "Community"))
  ) |>
  rename(pid = patient_site_id) |>
  # death_place supersedes it: the raw field mixed a place with a yes/no
  select(-c(community_death, dead_upon_arrival)) |>
  # pid is neither unique nor always filled (see the dupes check below), so
  # the key pairing it with the name is what identifies a case. Only the
  # serial drawn from that key is shared; the key never leaves this script.
  mutate(id_key = str_squish(paste(pid, nom))) |>
  mutate(unique_id = sprintf("BUT-%04d", cur_group_id()), .by = id_key) |>
  select(-id_key) |>
  relocate(unique_id)

cli::cli_alert_info(
  "{nrow(ll_narr_clean)} cases kept of {n_import}: \\
   {n_import - nrow(ll_narr_clean)} outside {toString(CONFIG$filter_hz)}"
)
cli::cli_alert_info(
  "{n_distinct(ll_narr_clean$unique_id)} unique_id across \\
   {nrow(ll_narr_clean)} rows"
)

#* Duplicate and missing pid ---------------
# review only, nothing dropped, so no denominator moves
# ! nom left out: the export must stay non-nominative
dupes_id <- ll_narr_clean |>
  filter(!is.na(pid)) |>
  janitor::get_dupes(pid) |>
  select(unique_id, pid, age, sex, adm2_comptabilisation)

cli::cli_alert_warning(
  "{nrow(dupes_id)} duplicated rows across {n_distinct(dupes_id$pid)} pid"
)
cli::cli_alert_warning("{sum(is.na(ll_narr_clean$pid))} rows with no pid")

rio::export(dupes_id, fs::path(tables_dir, "dupes_id.xlsx"))

#! Export the linelist: Donnees/propre is what the analysis scripts read, the
#! copy next to the raw export is for the team. Every column goes out, nom
#! included - prep_for_sharing.R is what de-identifies it for colleagues.
export_clean(ll_narr_clean, "linelist", time_write)
export_clean(ll_narr_clean, "linelist", time_write, dir = narr_ll_clean_dir)
export_clean(ll_narr_clean, "linelist", time_write, dir = local_ll_dir)

#* HEALTH FACILITY VISITS ------------------
# one row per visit
hf_visits <- ll_narr_clean |>
  select(
    unique_id,
    pid,
    contains("HF_name_visited"),
    contains("date_start_HF_visited"),
    contains("date_end_HF_visited")
  ) |>
  # character so all visit columns stack
  mutate(across(-c(unique_id, pid), as.character)) |>
  pivot_longer(
    cols = -c(unique_id, pid),
    names_to = c(".value", "visit"),
    names_pattern = "(.*?)(\\d+)$",
    names_transform = list(visit = as.integer)
  ) |>
  rename_with(\(x) str_remove(x, "_$")) |>
  mutate(across(where(is.character), \(x) na_if(str_squish(x), ""))) |>
  # drops the empty visit slots the wide layout leaves behind
  # filter(!is.na(HF_name_visited)) |>
  rename(hf_name = HF_name_visited) |>
  mutate(
    # "site | aire de santé | zone de santé" where the encoding is present
    hf_as = if_else(
      str_detect(hf_name, fixed("|")),
      str_squish(str_split_i(hf_name, fixed("|"), 2)),
      NA_character_
    ),
    hf_zs = if_else(
      str_detect(hf_name, fixed("|")),
      str_squish(str_split_i(hf_name, fixed("|"), -1)),
      NA_character_
    ),
    hf_name = str_squish(str_split_i(hf_name, fixed("|"), 1)),
    # the Graben clinic is recorded under several names
    hf_name = if_else(
      str_detect(hf_name, regex("graben|clinique", ignore_case = TRUE)),
      "UCG",
      hf_name
    ),
    across(c(date_start_HF_visited, date_end_HF_visited), as.Date),
    # raw export typo: 2030-05-26 entered for 2026-05-30
    across(
      c(date_start_HF_visited, date_end_HF_visited),
      \(x) if_else(x == as.Date("2030-05-26"), as.Date("2026-05-30"), x)
    ),
    los = as.numeric(date_end_HF_visited - date_start_HF_visited),
    # negative stay = data-entry error
    los = if_else(los < 0, NA_real_, los)
  ) |>
  # unique_id is the key app_data joins on; pid stays internal to prep
  select(-pid)

cli::cli_alert_info(
  "{nrow(hf_visits)} visits recorded by \\
   {n_distinct(hf_visits$unique_id)} of {nrow(ll_narr_clean)} cases"
)

export_clean(hf_visits, "hf-visits", time_write)
export_clean(hf_visits, "hf-visits", time_write, dir = local_hf_dir)

#* Save to server ------------------------------------------------------
# app_data.rds feeds the evd-2026-app dashboard on episerv
app_data <- list(
  linelist = ll_narr_clean,
  hf_visits = hf_visits,
  admin_data = list(adm1 = adm1, adm2 = adm2, adm3 = adm3)
)

app_data_path <- fs::path("R", "butembo_dashboard", "data", "app_data.rds")

saveRDS(app_data, app_data_path)

#* Send data to the server
system2(
  "rsync",
  args = c(
    "-zavh",
    fs::path_expand(app_data_path),
    "episerv:/home/epicentre/EVD-COD17-butembo/R/butembo_dashboard//data/"
  )
)
