# Clean the linelist, health-facility visits, alert and contact data.
# The only script that reads SharePoint. Exports to Donnees/propre and
# rsyncs app_data.rds to the evd-2026-app dashboard on episerv.

#* TODO ------------------------------------

#* CONFIG ------------------------------------

EXPORT_TO_SHAREPOINT <- TRUE
SEND_TO_SERVER <- TRUE
# FALSE reuses lab_data from the last app_data.rds, for when the lab exports are not to hand
PREP_LAB_DATA <- FALSE
# degrees, about 50 m; the app layers were slow at full detail. 0 keeps them as-is
GEO_SIMPLIFY_TOL <- 0.0005

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))
source(here::here("R", "fn_quality.R"))

#* Path ------------------------------------
time_write <- time_stamp()
app_data_path <- fs::path("R", "butembo_dashboard", "data", "app_data.rds")

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
    # isolation_site_id keeps the full string: it is the key into the HF matching table
    isolation_site_name = str_squish(str_split_i(
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
    # taken from the export field; a blank counts as not isolated
    isolated_etc = isolated %in% "Yes",

    # ! Which ETC ?
    # isolation_site_name is the part of isolation_site_id before the first "|"
    etc_site = if_else(isolated_etc, isolation_site_name, NA_character_),

    # ! Where was death ?
    death_place = case_when(
      outcome != "Died" ~ NA_character_,
      community_death == "CTE/CT" ~ "CTE/CT",
      community_death %in% c("ESS", "Communauté") ~ "Community",
      .default = NA_character_
    ),
    death_place = factor(death_place, levels = c("CTE/CT", "Community")),

    # ! Dead outside isolation ?
    # a recorded CTE/CT death stays inside isolation, even if flagged dead on arrival
    dead_out_isolation = case_when(
      # NA is kept for deaths with no data
      outcome != "Died" ~ "Not applicable",
      community_death == "CTE/CT" ~ "No",
      dead_upon_arrival %in% "Yes" ~ "Yes",
      community_death %in% c("ESS", "Communauté") ~ "Yes",
      # last, so it only fills deaths with no place recorded
      isolated %in% "No" ~ "Yes",
      .default = NA_character_
    ),
    dead_out_isolation = factor(
      dead_out_isolation,
      levels = c("Yes", "No", "Not applicable")
    )
  ) |>

  # Admin level code
  # ! # Standardise the admin levels - this makes file super large and laggy
  # ! ONSET
  left_join(
    select(adm1_nosf, adm1_name, adm1_pcode__onset = adm1_pcode),
    join_by(adm1_name__onset == adm1_name),
    relationship = "many-to-one"
  ) |>
  left_join(
    select(adm2_nosf, adm2_name, adm2_pcode__onset = adm2_pcode),
    join_by(adm2_name__onset == adm2_name),
    relationship = "many-to-one"
  ) |>
  left_join(
    select(adm3_nosf, adm3_name, adm3_pcode__onset = adm3_pcode, adm2_pcode),
    join_by(adm2_pcode__onset == adm2_pcode, adm3_name__onset == adm3_name),
    relationship = "many-to-one"
  ) |>

  # ! NOTIFICATION
  left_join(
    select(adm1_nosf, adm1_name, adm1_pcode__notif = adm1_pcode),
    join_by(adm1_name__notif == adm1_name),
    relationship = "many-to-one"
  ) |>
  left_join(
    select(adm2_nosf, adm2_name, adm2_pcode__notif = adm2_pcode),
    join_by(adm2_name__notif == adm2_name),
    relationship = "many-to-one"
  ) |>
  left_join(
    select(adm3_nosf, adm3_name, adm3_pcode__notif = adm3_pcode, adm2_pcode),
    join_by(adm3_name__notif == adm3_name, adm2_pcode__notif == adm2_pcode),
    relationship = "many-to-one"
  ) |>

  # ! RESIDENCE
  left_join(
    select(adm1_nosf, adm1_name, adm1_pcode__res = adm1_pcode),
    join_by(adm1_name__res == adm1_name),
    relationship = "many-to-one"
  ) |>
  left_join(
    select(adm2_nosf, adm2_name, adm2_pcode__res = adm2_pcode),
    join_by(adm2_name__res == adm2_name),
    relationship = "many-to-one"
  ) |>
  left_join(
    select(adm3_nosf, adm3_name, adm3_pcode__res = adm3_pcode, adm2_pcode),
    join_by(adm3_name__res == adm3_name, adm2_pcode__res == adm2_pcode),
    relationship = "many-to-one"
  ) |>
  # ! COMPTABILISATION
  left_join(
    select(adm2_nosf, adm2_name, adm2_pcode__comptabilisation = adm2_pcode),
    join_by(adm2_comptabilisation == adm2_name),
    relationship = "many-to-one"
  ) |>

  # after the joins, whose keys must keep their NA. Every grouping column
  # reads "Unknown" when empty, which the dashboard draws in light grey
  mutate(
    across(c(adm2_comptabilisation, isolated), \(x) replace_na(x, "Unknown")),
    across(
      c(age_group, sex, type_of_exit, dead_out_isolation),
      \(x) forcats::fct_na_value_to_level(x, "Unknown")
    ),
    # labelled copy for grouping; mod_vb needs dead_upon_notif to stay logical
    dead_upon_notif_grp = factor(
      case_match(
        dead_upon_notif,
        TRUE ~ "Dead on notification",
        FALSE ~ "Alive on notification",
        .default = "Unknown"
      ),
      levels = c("Dead on notification", "Alive on notification", "Unknown")
    ),
    transmission_type_1 = factor(
      case_match(
        transmission_type_1,
        c(NA, "Inconnu") ~ "Unknown",
        .default = transmission_type_1
      ),
      levels = c(
        "Familiale",
        "Funérailles",
        "Nosocomiale",
        "Sanguin directe",
        "Soins au malade",
        "Visite occasionnelle",
        "Autre",
        "Unknown"
      )
    )
  ) |>

  rename(pid = patient_site_id) |>
  select(-c(community_death, dead_upon_arrival)) |>
  mutate(id_key = str_squish(paste(pid, nom))) |>
  mutate(unique_id = sprintf("BUT-%04d", cur_group_id()), .by = id_key) |>
  select(-id_key) |>
  relocate(unique_id)


#* Transmission pairs ---------------------------------------------
transmission_data <- rio::import(fs::path(
  transmission_dir,
  "butembo_transmission_pairs.xlsx"
)) |>
  as_tibble() |>
  mutate(
    across(c(start_exposure, end_exposure), harmonize_dates),
    # match the type of pid, whichever way the export stored it
    id_infectee = if (is.character(ll_narr_clean$pid)) {
      as.character(id_infectee)
    } else {
      as.numeric(id_infectee)
    },
    only_exposure = tolower(as.character(only_exposure)) %in%
      c("yes", "oui", "true", "1"),
    most_probable = tolower(as.character(most_probable)) %in%
      c("yes", "oui", "true", "1")
  ) |>
  filter(!is.na(id_infectee))

# one pair per infectee: the most probable when several are listed
n_pairs <- nrow(transmission_data)
transmission_one <- transmission_data |>
  arrange(id_infectee, desc(most_probable)) |>
  slice(1, .by = id_infectee)
cli::cli_alert_info(
  "{n_pairs - nrow(transmission_one)} of {n_pairs} transmission pairs dropped: \\
   not the most probable for their infectee"
)
n_tie <- transmission_data |>
  filter(most_probable) |>
  count(id_infectee) |>
  filter(n > 1) |>
  nrow()
if (n_tie > 0) {
  cli::cli_alert_warning(
    "{n_tie} infectees have several most_probable pairs: first row kept"
  )
}

n_before <- nrow(ll_narr_clean)
ll_narr_clean <- ll_narr_clean |>
  left_join(
    transmission_one |>
      select(
        id_infectee,
        only_exposure,
        tr_infector_id = id_infector,
        tr_infector_name = name_infector,
        tr_start = start_exposure,
        tr_end = end_exposure
      ),
    by = join_by(pid == id_infectee),
    relationship = "many-to-one"
  ) |>
  mutate(
    # only_exposure pairs bring dates, not an infector
    tr_infector_id = if_else(
      !is.na(only_exposure) & only_exposure,
      NA,
      as.character(tr_infector_id)
    ),
    tr_infector_name = if_else(
      !is.na(only_exposure) & only_exposure,
      NA_character_,
      as.character(tr_infector_name)
    ),
    infector_ID_1 = dplyr::coalesce(
      tr_infector_id,
      as.character(infector_ID_1)
    ),
    infector_name_1 = dplyr::coalesce(tr_infector_name, infector_name_1),
    start_exposure_date_1 = dplyr::coalesce(tr_start, start_exposure_date_1),
    end_exposure_date_1 = dplyr::coalesce(tr_end, end_exposure_date_1)
  ) |>
  select(-c(only_exposure, starts_with("tr_"))) |>
  mutate(
    # factor, not logical, so the dashboard can group by it
    infector_identified = factor(
      if_else(is.na(infector_name_1), "No", "Yes"),
      levels = c("Yes", "No")
    )
  )
stopifnot("transmission join changed N" = nrow(ll_narr_clean) == n_before)
cli::cli_alert_info(
  "{n_before} -> {nrow(ll_narr_clean)} rows; {sum(transmission_one$id_infectee %in% ll_narr_clean$pid)} \\
   of {nrow(transmission_one)} infectees matched a pid, \\
   {sum(ll_narr_clean$infector_identified == 'Yes')} cases with an infector"
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
  select(
    pid,
    unique_id,
    adm2_comptabilisation,
    nom,
    age,
    age_unit,
    sex,
    date_notification,
    date_lab_result_1,
    lab_id_1,
    dupe_count
  )

cli::cli_alert_warning(
  "{nrow(dupes_id)} duplicated rows across {n_distinct(dupes_id$pid)} pid"
)
cli::cli_alert_warning("{sum(is.na(ll_narr_clean$pid))} rows with no pid")

#! export duplicates pid for removal
dupes_id |>
  arrange(pid) |>
  qxl::qxl(
    file = fs::path(tables_dir, "pid_duplicates.xlsx"),
    group = "pid",
    filter = TRUE
  )

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

# list of facilities
but_hf <- rio::import(latest_narr_ll, sheet = "facilities", skip = 2) |>
  as_tibble() |>
  # rows only: dropping empty columns would remove a field nobody filled
  janitor::remove_empty(which = c("rows", "cols"))

#! load the latest GIS geobase replace when identified
hf_gis_geojson <- fs::path(
  butembo_project_data_path,
  "spatiale",
  "hf_gis_butembo_20261023.geojson"
)

hf_gis <- sf::st_read(hf_gis_geojson, quiet = TRUE) |>
  janitor::clean_names()

# ! Add isolation sites (CTE / CT)

#*

#* Notification structure
hf_notification <- ll_narr_clean |>
  filter(!is.na(facility_notification)) |>
  transmute(
    unique_id,
    hf_name = str_squish(str_split_i(facility_notification, fixed("|"), 1)),
    notification = TRUE
  ) |>
  distinct()

#* Isolation structure
hf_isolation <- ll_narr_clean |>
  filter(!is.na(isolation_site_id)) |>
  transmute(
    unique_id,
    pid,
    # after the five HF_name_visited slots
    visit = 6L,
    HF_name_visited = str_squish(isolation_site_id),
    date_start_HF_visited = as.character(date_admission_eff),
    date_end_HF_visited = as.character(date_exit_eff),
    visit_type = "isolation"
  )

#* Visited structures
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
  mutate(
    across(where(is.character), \(x) na_if(str_squish(x), "")),
    visit_type = "visited"
  ) |>
  bind_rows(hf_isolation) |>
  arrange(unique_id, visit) |>
  rename(hf_name = HF_name_visited) |>
  mutate(
    hf_name = if_else(
      str_detect(hf_name, regex("non connu|inconnu", ignore_case = TRUE)),
      NA_character_,
      hf_name
    )
  ) |>
  mutate(
    raw_name = hf_name,
    # "site | aire de santé | zone de santé | pcode"
    hf_as = if_else(
      str_detect(hf_name, fixed("|")),
      str_squish(str_split_i(hf_name, fixed("|"), 2)),
      NA_character_
    ),
    hf_zs = if_else(
      str_detect(hf_name, fixed("|")),
      str_squish(str_split_i(hf_name, fixed("|"), 3)),
      NA_character_
    ),
    hf_pcode = if_else(
      str_detect(hf_name, fixed("|")),
      str_squish(str_split_i(hf_name, fixed("|"), 4)),
      NA_character_
    ),
    hf_pcode = na_if(hf_pcode, "Non trouvé"),
    hf_name = str_squish(str_split_i(hf_name, fixed("|"), 1)),
    # initials for the dashboard timeline, e.g. "CH La Guerison" -> "CLG"
    hf_abbr = purrr::map_chr(
      str_split(hf_name, "\\s+"),
      \(w) paste(str_to_upper(str_sub(w, 1, 1)), collapse = "")
    ),
    across(c(date_start_HF_visited, date_end_HF_visited), as.Date),

    los = as.numeric(date_end_HF_visited - date_start_HF_visited),
    # negative stay = data-entry error; same-day visit counts as 1 day
    los = if_else(los < 0, NA_real_, los + 1)
  ) |>
  left_join(
    hf_notification,
    by = join_by(unique_id, hf_name),
    na_matches = "never",
    relationship = "many-to-one"
  ) |>
  mutate(notification = coalesce(notification, FALSE)) |>
  # unique_id is the key app_data joins on; pid stays internal to prep
  select(-pid)

# isolation already recorded as a visit would count the case twice
n_before <- nrow(hf_visits)
hf_visits <- hf_visits |>
  mutate(
    .by = c(unique_id, hf_name),
    isolated_here = any(visit_type == "isolation"),
    keep = is.na(hf_name) |
      visit_type == "visited" |
      !any(visit_type == "visited")
  ) |>
  filter(keep) |>
  select(-keep)
cli::cli_alert_info(
  "{n_before - nrow(hf_visits)} isolation rows dropped: \\
   same structure already visited by the case"
)

n_notif_lost <- nrow(anti_join(
  hf_notification,
  hf_visits,
  by = join_by(unique_id, hf_name)
))
cli::cli_alert_info(
  "{n_notif_lost} of {nrow(hf_notification)} notifying structures are not \\
   among the case's visits, so carry no notification flag"
)

cli::cli_alert_info(
  "{sum(is.na(hf_visits$hf_name))} visits have no usable structure name"
)

cli::cli_alert_info(
  "{nrow(hf_visits)} visits recorded by \\
   {n_distinct(hf_visits$unique_id)} of {nrow(ll_narr_clean)} cases"
)

# ! Matching sucess
hf_visits |>
  filter(!is.na(raw_name)) |>
  mutate(
    status = case_when(
      is.na(hf_pcode) ~ "no pcode in name",
      hf_pcode %in% hf_gis$pcode ~ "matched",
      .default = "pcode not in geobase"
    )
  ) |>
  count(visit_type, status)

#* LAB DATA --------------------------------
# Separate lab database, not the linelist lab_*_1:2 slots. Path and layout TBC.
if (PREP_LAB_DATA) {
  # load
  lab_inrb <- rio::import(
    latest_inrb_lab_path,
    skip = 7
  ) |>
    as_tibble() |>
    clean_names() |>
    remove_empty(c("rows", "cols")) |>
    select(1:20)

  lab_inrb_clean <- lab_inrb |>
    transmute(
      lab_id = id_labo_numero_de_l_echantillon,
      first_sample = nouveau_prelevement_ou_reprelevement ==
        "Nouveau Prélèvement",
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
    ) |>
    distinct(lab_id, lab_result, .keep_all = TRUE)

  lab_mobile <- rio::import(latest_mobile_lab) |>
    as_tibble() |>
    janitor::clean_names() |>
    janitor::remove_empty()

  lab_mobile_clean <- lab_mobile |>
    transmute(
      lab_id = id_labo_numero_de_l_echantillon,
      first_sample = nouveau_prelevement_ou_reprelevement ==
        "Nouveau Prélèvement",
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
    ) |>
    distinct(lab_id, lab_result, .keep_all = TRUE)

  lab_data <- bind_rows(lab_mobile_clean, lab_inrb_clean)

  # a future result date is a typo and would anchor the dashboard's latest-result and period filters
  n_future <- sum(lab_data$date_lab_result > Sys.Date(), na.rm = TRUE)
  lab_data$date_lab_result[
    lab_data$date_lab_result > Sys.Date() & !is.na(lab_data$date_lab_result)
  ] <- NA
  cli::cli_alert_info("date_lab_result: {n_future} future dates set to NA")

  lab_data <- lab_data |>
    mutate(
      # "Unknown" stays out of positivity, which tests %in% Positif/Négatif
      lab_result = factor(
        replace_na(lab_result, "Unknown"),
        levels = c("Négatif", "Positif", "Unknown")
      ),
      sample_type = replace_na(sample_type, "Unknown"),
      source = replace_na(source, "Unknown"),
      first_sample = factor(
        case_match(first_sample, TRUE ~ "Oui", FALSE ~ "Non", NA ~ "Unknown"),
        levels = c("Oui", "Non", "Unknown")
      ),
      delay_sampling_result_days = as.numeric(date_lab_result - date_sampling),
      delay_notif_result_days = as.numeric(date_lab_result - date_notification)
    )

  # negative delays are entry errors; over 15 days is implausible for a lab turnaround
  max_lab_delay <- 15
  for (col in c("delay_sampling_result_days", "delay_notif_result_days")) {
    n_valid_before <- sum(!is.na(lab_data[[col]]))
    invalid <- !is.na(lab_data[[col]]) &
      (lab_data[[col]] < 0 | lab_data[[col]] > max_lab_delay)
    lab_data[[col]][invalid] <- NA_real_
    cli::cli_alert_info(
      "{col}: {sum(invalid)} of {n_valid_before} set to NA (negative or > {max_lab_delay} days)"
    )
  }
} else {
  # reuse the previous export, so the Lab tab keeps its data
  stopifnot(
    "no previous app_data.rds to reuse lab_data from" = fs::file_exists(
      app_data_path
    )
  )
  lab_data <- readRDS(app_data_path)$lab_data
  cli::cli_alert_info(
    "lab data skipped: {nrow(lab_data)} rows reused from {fs::path_file(app_data_path)}"
  )
}


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
  hf_gis = sf::st_transform(hf_gis, 4326),
  lab_data = lab_data,
  admin_data = list(adm1 = adm1, adm2 = adm2, adm3 = adm3)
)

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

#* Summary of app_data ---------------------
app_tables <- app_data[vapply(app_data, is.data.frame, logical(1))]
app_tables_msg <- purrr::imap_chr(
  app_tables,
  \(x, nm) paste0(nm, ": ", nrow(x), " rows x ", ncol(x), " cols")
)
admin_msg <- purrr::imap_chr(
  app_data$admin_data,
  \(x, nm) paste0(nm, " (", nrow(x), ")")
)
cli::cli_h2(
  "app_data: {fs::path_file(app_data_path)}, {fs::file_size(app_data_path)}"
)
cli::cli_bullets(c(
  rlang::set_names(app_tables_msg, rep("i", length(app_tables_msg))),
  "i" = "quality: {length(app_data$quality)} elements",
  "i" = "admin_data features: {toString(admin_msg)}",
  if (SEND_TO_SERVER) {
    c("v" = "sent to episerv")
  } else {
    c("!" = "not sent to episerv")
  }
))
