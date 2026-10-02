# Clean the linelist, health-facility visits, alert and contact data.
# The only script that reads SharePoint. Exports to Donnees/propre and
# rsyncs app_data.rds to the evd-2026-app dashboard on episerv.

#* TODO ------------------------------------

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
    # both sites took ordinary patients before these dates
    isolated_etc = case_when(
      isolation_site_name == "HGR Katwa" &
        date_admission_eff >= as.Date("2026-06-15") &
        # unknown counts as alive at notification, so the cohort keeps them
        !dead_upon_notif %in% TRUE ~ TRUE,
      # an MSF id also marks a Kitatumba admission, whatever the date
      isolation_site_name %in%
        c("HGR Kitatumba", "CTE Kitatumba") &
        (date_admission_eff >= as.Date("2026-07-01") | !is.na(id_msf)) &
        !dead_upon_notif %in% TRUE ~ TRUE,
      .default = FALSE
    ),

    # ! Which ETC ?
    etc_site = case_when(
      isolated_etc & isolation_site_name == "HGR Katwa" ~ "CTE Katwa (MEDAIR)",
      isolated_etc &
        isolation_site_name %in% c("HGR Kitatumba", "CTE Kitatumba") ~
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
if (!fs::file_exists(local_hf_geo_csv)) {
  fs::file_copy(hf_geo_csv, local_hf_geo_csv)
}
hf_geo_raw <- rio::import(local_hf_geo_csv) |>
  as_tibble() |>
  clean_names() |>
  rename(raw_name = isolation_site_id)

# x/y are Web Mercator metres; the app and flow mapper need lon/lat
hf_xy <- hf_geo_raw |>
  filter(!is.na(x), !is.na(y)) |>
  select(raw_name, hf_pcode = pcode, x, y)
hf_lonlat <- hf_xy |>
  st_as_sf(coords = c("x", "y"), crs = 3857) |>
  st_transform(4326)
hf_xy <- hf_xy |>
  mutate(
    lon = st_coordinates(hf_lonlat)[, 1],
    lat = st_coordinates(hf_lonlat)[, 2]
  ) |>
  select(raw_name, hf_pcode, lon, lat)
cli::cli_alert_info(
  "{nrow(hf_xy)} of {nrow(hf_geo_raw)} structures in the matching table have coordinates"
)

# isolation site as a final visit, same layout as the pivoted visit slots
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
  mutate(visit_type = "visited") |>
  bind_rows(hf_isolation) |>
  arrange(unique_id, visit) |>
  # drops the empty visit slots the wide layout leaves behind
  # filter(!is.na(HF_name_visited)) |>
  rename(hf_name = HF_name_visited) |>
  mutate(
    # untouched export string: the key into the matching table
    raw_name = hf_name,
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

# some cases list the whole pathway up to isolation, others stop before it
hf_last_visited <- hf_visits |>
  filter(visit_type == "visited", !is.na(hf_name)) |>
  summarise(.by = unique_id, last_visited_name = last(hf_name))
n_before <- nrow(hf_visits)
n_iso <- sum(hf_visits$visit_type == "isolation")
hf_visits <- hf_visits |>
  left_join(
    hf_last_visited,
    by = join_by(unique_id),
    relationship = "many-to-one"
  ) |>
  # isolation already recorded as the last visit would count the case twice
  filter(
    !(visit_type == "isolation" &
      coalesce(hf_name == last_visited_name, FALSE))
  ) |>
  select(-last_visited_name)
cli::cli_alert_info(
  "{n_iso} isolation rows added; {n_before - nrow(hf_visits)} dropped as \\
   the last recorded visit, {n_iso - (n_before - nrow(hf_visits))} kept"
)


cli::cli_alert_info(
  "{nrow(hf_visits)} visits recorded by \\
   {n_distinct(hf_visits$unique_id)} of {nrow(ll_narr_clean)} cases"
)

# visits with no structure name cannot be counted or linked
n_before <- nrow(hf_visits)
hf_named <- hf_visits |>
  filter(!is.na(raw_name))
cli::cli_alert_info(
  "{n_before - nrow(hf_named)} of {n_before} visits dropped: no structure name"
)

#* Cases per structure, geo-matched ----------
hf_cases <- hf_named |>
  summarise(
    .by = c(raw_name, hf_name, hf_as, hf_zs),
    n_cases = n_distinct(unique_id)
  )
n_before <- nrow(hf_cases)
hf_cases <- hf_cases |>
  left_join(
    hf_xy,
    by = join_by(raw_name),
    relationship = "many-to-one"
  )
stopifnot(nrow(hf_cases) == n_before)

# finest level at which each structure can be placed; "AS non définie" is a placeholder
HF_PRECISION_LEVELS <- c(
  "Health facility",
  "Health area",
  "Health zone",
  "Not located"
)
hf_cases <- hf_cases |>
  mutate(
    precision = case_when(
      !is.na(lon) ~ "Health facility",
      !is.na(hf_as) & hf_as != "AS non définie" ~ "Health area",
      !is.na(hf_zs) ~ "Health zone",
      .default = "Not located"
    ),
    precision = factor(precision, levels = HF_PRECISION_LEVELS)
  )

cli::cli_alert_info(
  "{sum(is.na(hf_cases$lon))} of {nrow(hf_cases)} structures \\
   ({sum(hf_cases$n_cases[is.na(hf_cases$lon)])} case-visits) have no coordinates"
)

#* Flows between structures, geo-matched -----
# consecutive visits per case, ordered by start date then visit slot
hf_flows <- hf_named |>
  arrange(unique_id, date_start_HF_visited, visit) |>
  mutate(
    from = raw_name,
    to = lead(raw_name),
    .by = unique_id
  ) |>
  filter(!is.na(to))
n_pairs <- nrow(hf_flows)
hf_flows <- hf_flows |>
  filter(from != to)
cli::cli_alert_info(
  "{n_pairs - nrow(hf_flows)} of {n_pairs} consecutive pairs dropped: same structure twice"
)
hf_flows <- hf_flows |>
  summarise(.by = c(from, to), n_cases = n_distinct(unique_id))
n_before <- nrow(hf_flows)
hf_flows <- hf_flows |>
  left_join(
    hf_xy |> rename_with(\(x) paste0("from_", x), -raw_name),
    by = join_by(from == raw_name),
    relationship = "many-to-one"
  ) |>
  left_join(
    hf_xy |> rename_with(\(x) paste0("to_", x), -raw_name),
    by = join_by(to == raw_name),
    relationship = "many-to-one"
  )
stopifnot(nrow(hf_flows) == n_before)
cli::cli_alert_info(
  "{sum(is.na(hf_flows$from_lon) | is.na(hf_flows$to_lon))} of {n_before} \\
   flows lack coordinates at one end and cannot be mapped"
)


# compare to GIS flow
hf_flow_gis <- sf::st_read(hf_flow_gis_path, quiet = TRUE)
cli::cli_alert_info(
  "hf_flow_gis: {nrow(hf_flow_gis)} features, {unique(as.character(sf::st_geometry_type(hf_flow_gis)))}"
)

hf_flow_gis |>
  filter(hf_depart == "HGR Kitatumba")

hf_flows |>
  filter(str_detect(from, "HGR Kitatumba")) |>
  select(from, to)


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
adm1 <- prep_app_layer(adm1)
adm2 <- prep_app_layer(adm2)
adm3 <- prep_app_layer(adm3)

#* Data-quality tables (Data quality tab) ----------------------------------
quality <- build_quality(ll_narr_clean)

# visited structures that could not be mapped, as the Health facilities pane reads them
quality$hf_unmatched <- hf_named |>
  anti_join(hf_xy, by = join_by(raw_name)) |>
  summarise(
    .by = c(raw_name, hf_name, hf_as, hf_zs),
    `N visits` = n(),
    `N cases` = n_distinct(unique_id)
  ) |>
  left_join(
    select(hf_cases, raw_name, Precision = precision),
    by = join_by(raw_name),
    relationship = "many-to-one"
  ) |>
  mutate(
    Reason = if_else(
      raw_name %in% hf_geo_raw$raw_name,
      "In matching table, no coordinates",
      "Not in matching table"
    )
  ) |>
  select(
    Structure = hf_name,
    `Health area` = hf_as,
    `Health zone` = hf_zs,
    Precision,
    Reason,
    `N visits`,
    `N cases`
  ) |>
  arrange(Precision, desc(`N visits`))

# structures, visits and cases by finest geographic level reached
hf_precision <- hf_named |>
  left_join(
    select(hf_cases, raw_name, precision),
    by = join_by(raw_name),
    relationship = "many-to-one"
  ) |>
  summarise(
    .by = precision,
    n_structures = n_distinct(raw_name),
    n_visits = n(),
    n_cases = n_distinct(unique_id)
  ) |>
  arrange(precision)
stopifnot(sum(hf_precision$n_visits) == nrow(hf_named))

quality$hf_summary <- list(
  n_total = nrow(hf_cases),
  n_matched = sum(!is.na(hf_cases$lon)),
  precision = hf_precision
)
stopifnot(
  nrow(quality$hf_unmatched) ==
    quality$hf_summary$n_total -
      quality$hf_summary$n_matched
)

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
  hf_cases = hf_cases,
  hf_flows = hf_flows,
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
