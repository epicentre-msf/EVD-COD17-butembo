# Clean the linelist, health-facility visits, alert and contact data.
# The only script that reads SharePoint. Exports to Donnees/propre and
# rsyncs app_data.rds to the evd-2026-app dashboard on episerv.

#* TODO ------------------------------------
# - [ ] lab tab: set lab_path in 0_global.R, import and clean lab data

#* CONFIG ------------------------------------

EXPORT_TO_SHAREPOINT <- TRUE
SEND_TO_SERVER <- TRUE
# degrees, about 50 m; the app layers were slow at full detail. 0 keeps them as-is
GEO_SIMPLIFY_TOL <- 0.0005

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))
source(here::here("R", "fn_quality.R"))

#* Path ------------------------------------
time_write <- time_stamp()

#* Local geo cache --------------------------

#load and save to local the geobase from sharepoint -- silenced because of size
# adm1 <- readRDS(fs::path(sf_data_path, "COD_adm1.rds"))
# saveRDS(adm1, fs::path(local_geobase_dir, "COD_adm1.rds"))
# adm1_sub <- readRDS(fs::path(sf_data_path, "COD_adm1_sub.rds"))
# saveRDS(adm1, fs::path(local_geobase_dir, "COD_adm1_sub.rds"))

# # adm2
# adm2 <- readRDS(fs::path(sf_data_path, "COD_adm2.rds"))
# saveRDS(adm2, fs::path(local_geobase_dir, "COD_adm2.rds"))
# adm2_sub <- readRDS(fs::path(sf_data_path, "COD_adm2_sub.rds"))
# saveRDS(adm2, fs::path(local_geobase_dir, "COD_adm2_sub.rds"))

# # adm3
# adm3 <- readRDS(fs::path(sf_data_path, "COD_adm3.rds"))
# saveRDS(adm3, fs::path(local_geobase_dir, "COD_adm3.rds"))
# adm3_sub <- readRDS(fs::path(sf_data_path, "COD_adm3_sub.rds"))
# saveRDS(adm3, fs::path(local_geobase_dir, "COD_adm3_sub.rds"))

#* Geobase -----------------------------
adm1 <- readRDS(fs::path(local_geobase_dir, "COD_adm1.rds"))
adm2 <- readRDS(fs::path(local_geobase_dir, "COD_adm2.rds"))
adm3 <- readRDS(fs::path(local_geobase_dir, "COD_adm3.rds"))

# drop geometry
adm1_nosf <- adm1 |> st_drop_geometry() |> select(adm1_name, adm1_pcode)
adm2_nosf <- adm2 |>
  st_drop_geometry() |>
  select(adm1_name, adm2_name, adm2_pcode)
adm3_nosf <- adm3 |>
  st_drop_geometry() |>
  select(adm1_name, adm2_name, adm3_name, adm3_pcode, adm2_pcode)

#* Import data -----------------------------
ll_narr <- rio::import(latest_narr_ll, sheet = "data", skip = 2) |>
  as_tibble() |>
  # rows only: dropping empty columns would remove a field nobody filled
  janitor::remove_empty(which = c("rows", "cols"))

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

    #! Place of residence / onset
    adm1_name__onset = case_when(
      res_equal_onset == "Yes" ~ adm1_name__res,
      .default = adm1_name__onset
    ),
    adm2_name__onset = case_when(
      res_equal_onset == "Yes" ~ adm2_name__res,
      .default = adm2_name__onset
    ),
    adm3_name__onset = case_when(
      res_equal_onset == "Yes" ~ adm3_name__res,
      .default = adm3_name__onset
    ),

    #! Place of Notification
    adm2_name__notif = case_when(
      is.na(adm2_name__notif) ~ adm2_comptabilisation,
      .default = adm2_name__notif
    ),

    across(contains("adm1"), ~ str_remove(.x, "COD ")),

    # source mixes KATWA / katwa, which splits groups and breaks the zone joins
    across(
      c(adm2_comptabilisation, adm2_name__onset, adm2_name__notif),
      str_to_sentence
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

  # Admin level code
  # ! # Standardise the admin levels - this makes file super large and laggy
  # ! ONSET
  left_join(
    select(adm1_nosf, adm1_name, adm1_pcode__onset = adm1_pcode),
    join_by(adm1_name__onset == adm1_name)
  ) |>
  left_join(
    select(adm2_nosf, adm2_name, adm2_pcode__onset = adm2_pcode),
    join_by(adm2_name__onset == adm2_name)
  ) |>
  left_join(
    select(adm3_nosf, adm3_name, adm3_pcode__onset = adm3_pcode, adm2_pcode),
    join_by(adm2_pcode__onset == adm2_pcode, adm3_name__onset == adm3_name)
  ) |>

  # ! NOTIFICATION
  left_join(
    select(adm1_nosf, adm1_name, adm1_pcode__notif = adm1_pcode),
    join_by(adm1_name__notif == adm1_name)
  ) |>
  left_join(
    select(adm2_nosf, adm2_name, adm2_pcode__notif = adm2_pcode),
    join_by(adm2_name__notif == adm2_name)
  ) |>
  left_join(
    select(adm3_nosf, adm3_name, adm3_pcode__notif = adm3_pcode, adm2_pcode),
    join_by(adm3_name__notif == adm3_name, adm2_pcode__notif == adm2_pcode)
  ) |>

  # ! RESIDENCE
  left_join(
    select(adm1_nosf, adm1_name, adm1_pcode__res = adm1_pcode),
    join_by(adm1_name__res == adm1_name)
  ) |>
  left_join(
    select(adm2_nosf, adm2_name, adm2_pcode__res = adm2_pcode),
    join_by(adm2_name__res == adm2_name)
  ) |>
  left_join(
    select(adm3_nosf, adm3_name, adm3_pcode__res = adm3_pcode, adm2_pcode),
    join_by(adm3_name__res == adm3_name, adm2_pcode__res == adm2_pcode)
  ) |>
  # ! COMPTABILISATION
  left_join(
    select(adm2_nosf, adm2_name, adm2_pcode__comptabilisation = adm2_pcode),
    join_by(adm2_comptabilisation == adm2_name)
  ) |>

  rename(pid = patient_site_id) |>
  select(-c(community_death, dead_upon_arrival)) |>
  mutate(id_key = str_squish(paste(pid, nom))) |>
  mutate(unique_id = sprintf("BUT-%04d", cur_group_id()), .by = id_key) |>
  select(-id_key) |>
  relocate(unique_id)

cli::cli_alert_info(
  "{nrow(ll_narr_clean)} cases kept of {n_import}: \\
   {n_import - nrow(ll_narr_clean)} outside {toString(CONFIG$filter_hz)}"
)
# adm3_name__res unmatched against the geobase: no pcode, so no map join
n_res_unmatched <- with(
  ll_narr_clean,
  sum(!is.na(adm3_name__res) & is.na(adm3_pcode__res))
)
cli::cli_alert_warning(
  "{n_res_unmatched} rows have adm3_name__res with no matching adm3_pcode__res"
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

#! Export the linelist: Donnees/ in sharepoint

if (EXPORT_TO_SHAREPOINT) {
  saveRDS(
    ll_narr_clean,
    fs::path(
      narr_ll_clean_dir,
      glue::glue("{CONFIG$export_prefix}_linelist__{time_write}.rds")
    )
  )
}
# export local version
saveRDS(
  ll_narr_clean,
  fs::path(
    local_ll_dir,
    glue::glue("{CONFIG$export_prefix}_linelist__{time_write}.rds")
  )
)


#* HEALTH FACILITY VISITS ------------------

# copied locally once; delete the local file to refresh from SharePoint
if (!fs::file_exists(local_hf_cases_json)) {
  fs::file_copy(hf_cases_json, local_hf_cases_json)
}
hf_matched <- sf::read_sf(local_hf_cases_json)

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
    names_transform = list(visit = as.integer),
    values_drop_na = TRUE,
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
    # initials for the dashboard timeline, e.g. "CH La Guerison" -> "CLG"
    hf_abbr = purrr::map_chr(
      str_split(hf_name, "\\s+"),
      \(w) paste(str_to_upper(str_sub(w, 1, 1)), collapse = "")
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

#* LAB DATA --------------------------------
# Separate lab database, not the linelist lab_*_1:2 slots. Path and layout TBC.

# load
lab_inrb <- rio::import(latest_inrb_lab_path, skip = 7) |>
  as_tibble() |>
  clean_names() |>
  remove_empty()


lab_inrb_clean <- lab_inrb |>
  transmute(
    new_sample = nouveau_prelevement_ou_reprelevement == "Nouveau Prélèvement",
    date_notification = ymd(date_de_notification_dd_mm_yyy),
    patient_status = statut_du_patient_au_moment_collecte_echantillon_dcd_vivant,
    province,
    zone_de_sante,
    aire_de_sante,
    origin = provenance,
    sample_type = recode_values(
      type_d_echantillon,
      "Ecouvillon Bucal(Oral,Salive)" ~ "Ecouvillon oral",
      c("sang total", "Sang total") ~ "Sang total",
      c("Lait Matérnel") ~ "Lait Maternel",
      NA ~ NA_character_
    ),
    date_sampling = ymd(date_de_prelevement_dd_mm_yyy),
    date_lab_result = ymd(date_d_analyse_dd_mm_yyy),
    lab_result = recode_values(
      resultat_final,
      "NEGATIF" ~ "Négatif",
      "POSITIF" ~ "Positif",
      NA ~ NA_character_
    ),
    source = "INRB Béni"
  )

lab_mobile <- rio::import(latest_mobile_lab) |>
  as_tibble() |>
  clean_names() |>
  remove_empty()

lab_mobile_clean <- lab_mobile |>
  transmute(
    new_sample = nouveau_prelevement_ou_reprelevement == "Nouveau Prélèvement",
    patient_status = recode_values(
      statut_du_patient_au_moment_collecte_echantillon_dcd_vivant,
      c("Décédé") ~ "Décédé",
      "Vivant" ~ "Vivant",
      NA ~ NA_character_
    ),
    date_notification = harmonize_dates(date_de_notification_dd_mm_yyy),
    province,
    zone_de_sante,
    aire_de_sante,
    origin = provenance,
    sample_type = recode_values(
      type_d_echantillon,
      "Ecouvillon Oral" ~ "Ecouvillon oral",
      c("sang total", "Sang total", "Sérum", "Sang") ~ "Sang total",
      c("Lait Matérnel") ~ "Lait Maternel",
      NA ~ NA_character_
    ),
    date_sampling = harmonize_dates(date_de_prelevement_mm_dd_yyy),
    date_lab_result = harmonize_dates(date_danalyse),
    lab_result = case_when(
      kit_danalyse_altona_filoscreen_1_0_resultats_pos_neg %in%
        c(
          "Negatif",
          "Negatit",
          "Négatif",
          "negatif",
          "Negatif MVE mais positif Rickettsia Salmonella",
          "Negatif MVE mais positif goutte epaisse"
        ) ~ "Négatif",
      kit_danalyse_altona_filoscreen_1_0_resultats_pos_neg %in%
        c("Positf", "Positif") ~ "Positif",
      .default = NA_character_
    ),
    source = "Laboratoire Mobile"
  )

lab_data <- bind_rows(lab_mobile_clean, lab_inrb_clean) |>
  mutate(lab_result = factor(lab_result, levels = c("Négatif", "Positif")))

#* Prepare dashboard data -------------------------------------------

# all pcodes needed for the data
adm1_pcode_used <- unique(na.omit(c(
  ll_narr_clean$adm1_pcode__onset,
  ll_narr_clean$adm1_pcode__notif,
  ll_narr_clean$adm1_pcode__res
)))

adm2_pcode_used <- unique(na.omit(c(
  ll_narr_clean$adm2_pcode__onset,
  ll_narr_clean$adm2_pcode__notif,
  ll_narr_clean$adm2_pcode__comptabilisation,
  ll_narr_clean$adm2_pcode__res
)))

adm3_pcode_used <- unique(na.omit(c(
  ll_narr_clean$adm3_pcode__onset,
  ll_narr_clean$adm3_pcode__notif,
  ll_narr_clean$adm3_pcode__res
)))

adm1 <- adm1 |>
  filter(adm1_pcode %in% adm1_pcode_used)

adm2 <- adm2 |>
  filter(adm2_pcode %in% adm2_pcode_used)

adm3 <- adm3 |>
  filter(adm3_pcode %in% adm3_pcode_used)

# the app draws on a web basemap, so it needs lon/lat; done once here
prep_app_layer <- function(x) {
  x <- st_transform(x, 4326)
  if (GEO_SIMPLIFY_TOL > 0) {
    x <- st_simplify(x, dTolerance = GEO_SIMPLIFY_TOL, preserveTopology = TRUE)
  }
  x
}
adm1 <- prep_app_layer(adm1)
adm2 <- prep_app_layer(adm2)
adm3 <- prep_app_layer(adm3)

#* Data-quality tables (Data quality tab) ----------------------------------
quality <- build_quality(ll_narr_clean)

# variables with no section, or beyond the first two HF slots, are not shown
n_quality_hidden <- length(setdiff(
  setdiff(names(ll_narr_clean), QUALITY_EXCLUDE_VARS),
  quality$completeness$variable
))
cli::cli_alert_info(
  "{nrow(quality$completeness)} variables in the completeness table, \\
   {n_quality_hidden} left out (no section or excluded)"
)

# dates the delay module pairs up, chronological so delays come out positive
delay_dates <- c(
  "date_symptom_onset",
  "date_notification",
  "date_admission_eff",
  "date_lab_result_1",
  "date_exit_eff"
)

#* Save to server ------------------------------------------------------
# app_data.rds feeds the evd-2026-app dashboard on episerv
app_data <- list(
  linelist = add_delay_pairs(ll_narr_clean, delay_dates),
  quality = quality,
  hf_visits = hf_visits,
  lab_data = lab_data,
  admin_data = list(adm1 = adm1, adm2 = adm2, adm3 = adm3)
)

app_data_path <- fs::path("R", "butembo_dashboard", "data", "app_data.rds")

saveRDS(app_data, app_data_path)

if (SEND_TO_SERVER) {
  #* Send data to the server
  system2(
    "rsync",
    args = c(
      "-zavh",
      fs::path_expand(app_data_path),
      "episerv:/home/epicentre/EVD-COD17-butembo/R/butembo_dashboard/data/"
    )
  )
}
