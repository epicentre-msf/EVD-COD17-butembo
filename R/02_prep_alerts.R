# Import, standardise and stack the alert registers of the three health zones.
# Reads the latest register per zone; writes alerts_clean.rds for alerts_analysis.R.

#* TODO ------------------------------------
# - [ ] fill the Katwa and Butembo blocks (sheet, skip, header map)
# - [ ] fill the health_area recode table once unmatched names are listed
# - [ ] confirm whether Beni and Butembo report daily or weekly

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))

time_write <- time_stamp()

#* Musienene -----------------------------
latest_mus_alert <- fs::dir_ls(fs::path(alert_dir, "musienene")) |> max()

# Excel headers are positional leftovers; all_of() errors if the layout shifts.
mus_alert_names <- c(
  date = "date",
  health_zone = "zone_sante_2",
  health_area = "aire_sante",
  alert_alive = "v4",
  alert_dead = "d5",
  valid_alive = "v10",
  valid_dead = "d11",
  invalid_alive = "v12",
  invalid_dead = "d13",
  alert_source_fosa = "fosa",
  alert_source_ra = "ra",
  alert_source_comm = "communaute",
  alert_source_poc = "po_c"
)

alert_mus <- rio::import(latest_mus_alert, which = "data", skip = 1) |>
  as_tibble() |>
  clean_names() |>
  select(all_of(mus_alert_names)) |>
  mutate(health_zone = "musienene", source_system = "register")

#* Katwa - -----------------------------
#* linelist format 1 row per alert
latest_kat_alert <- fs::dir_ls(fs::path(alert_dir, "katwa")) |> max()

kat_alert_names <- c(
  date = "date",
  health_area = "aire_de_sante",
  alert_source = "source",
  alert_status = "statut_11",
  valid_status = "statut_12"
)

alert_kat_raw <- rio::import(
  latest_kat_alert,
  which = "Alertes du 04",
  skip = 0
) |>
  as_tibble() |>
  clean_names() |>
  select(all_of(kat_alert_names))

# clean linelist
alert_kat_ll <- alert_kat_raw |>
  mutate(
    date = harmonize_dates(date),
    health_zone = "katwa",
    health_area = tolower(str_squish(health_area)),
    across(where(is.character), \(x) na_if(str_squish(x), "")),
    alert_status = case_match(
      alert_status,
      "Décès" ~ "dead",
      "Vivant" ~ "alive"
    ),
    valid_status = case_match(
      valid_status,
      "Invalidée" ~ "invalid",
      "Validée" ~ "valid",
      .default = NA_character_
    ),
    alert_source = case_match(
      alert_source,
      "CT" ~ "fosa",
      "CTE" ~ "fosa",
      "ESS" ~ "fosa",
      "RA" ~ "ra",
      "SUIVI DES CONTACTS" ~ "contact_tracing",
      "SBC" ~ "comm",
      .default = NA_character_
    )
  ) |>
  select(
    date,
    health_zone,
    health_area,
    alert_status,
    valid_status,
    alert_source
  )

# Katwa - aggregate to the register format
# Days with no alert get 0, only between an area's first and last alert.
kat_count_cols <- c(
  "alert_alive",
  "alert_dead",
  "valid_alive",
  "valid_dead",
  "invalid_alive",
  "invalid_dead",
  "alert_source_fosa",
  "alert_source_ra",
  "alert_source_comm",
  "alert_source_poc"
)

alert_kat <- alert_kat_ll |>
  filter(!is.na(date), !is.na(health_area)) |>
  summarise(
    .by = c(health_zone, health_area, date),
    alert_alive = sum(alert_status == "alive", na.rm = TRUE),
    alert_dead = sum(alert_status == "dead", na.rm = TRUE),
    valid_alive = sum(
      valid_status == "valid" & alert_status == "alive",
      na.rm = TRUE
    ),
    valid_dead = sum(
      valid_status == "valid" & alert_status == "dead",
      na.rm = TRUE
    ),
    invalid_alive = sum(
      valid_status == "invalid" & alert_status == "alive",
      na.rm = TRUE
    ),
    invalid_dead = sum(
      valid_status == "invalid" & alert_status == "dead",
      na.rm = TRUE
    ),
    alert_source_fosa = sum(alert_source == "fosa", na.rm = TRUE),
    alert_source_ra = sum(alert_source == "ra", na.rm = TRUE),
    alert_source_comm = sum(alert_source == "comm", na.rm = TRUE),
    alert_source_poc = sum(alert_source == "poc", na.rm = TRUE)
  ) |>
  dplyr::reframe(
    .by = c(health_zone, health_area),
    tidyr::complete(
      dplyr::pick(everything()),
      date = seq(min(date), max(date), by = "day")
    )
  ) |>
  mutate(across(all_of(kat_count_cols), \(x) tidyr::replace_na(x, 0L))) |>
  mutate(source_system = "linelist")

# Counted alerts must equal alerts with a usable date and area.
stopifnot(
  "katwa aggregation lost alerts" = sum(
    alert_kat$alert_alive + alert_kat$alert_dead
  ) ==
    sum(
      !is.na(alert_kat_ll$alert_status) &
        !is.na(alert_kat_ll$date) &
        !is.na(alert_kat_ll$health_area)
    )
)

#* Butembo ------------------------------------------------------------------------------------

latest_butembo_alert <- fs::dir_ls(fs::path(alert_dir, "butembo")) |> max()

but_alert_names <- c(
  date = "date_notif",
  health_area = "as",
  alert_source = "alerte_source",
  alert_status = "status_initial",
  valid_status = "alert_conc"
)

alert_but_raw <- rio::import(latest_butembo_alert, which = "Base", skip = 2) |>
  as_tibble() |>
  clean_names() |>
  select(all_of(but_alert_names))

# clean linelist
alert_but_ll <- alert_but_raw |>
  mutate(
    date = harmonize_dates(date),
    health_zone = "butembo",
    health_area = tolower(str_squish(health_area)),
    across(where(is.character), \(x) na_if(str_squish(x), "")),
    alert_status = case_match(
      alert_status,
      "Décédé" ~ "dead",
      "Vivant" ~ "alive",
      .default = NA_character_
    ),
    valid_status = case_match(
      valid_status,
      "Invalidée" ~ "invalid",
      "Validée" ~ "valid",
      .default = NA_character_
    ),
    alert_source = case_match(
      alert_source,
      "Structure Sanitaire" ~ "fosa",
      "Morgue" ~ "fosa",
      "Recherche active" ~ "ra",
      "Pointe d'entrée" ~ "poc",
      "Suivi des contacts" ~ "contact_tracing",
      "Agent communautaire" ~ "comm",
      "Communauté/Famille" ~ "comm",
      "Communication/SBC" ~ "comm",
      .default = NA_character_
    )
  ) |>
  select(
    date,
    health_zone,
    health_area,
    alert_status,
    valid_status,
    alert_source
  )

# Butembo - aggregate to the register format
# Days with no alert get 0, only between an area's first and last alert.
but_count_cols <- c(
  "alert_alive",
  "alert_dead",
  "valid_alive",
  "valid_dead",
  "invalid_alive",
  "invalid_dead",
  "alert_source_fosa",
  "alert_source_ra",
  "alert_source_comm",
  "alert_source_poc"
)

alert_but <- alert_but_ll |>
  filter(!is.na(date), !is.na(health_area)) |>
  summarise(
    .by = c(health_zone, health_area, date),
    alert_alive = sum(alert_status == "alive", na.rm = TRUE),
    alert_dead = sum(alert_status == "dead", na.rm = TRUE),
    valid_alive = sum(
      valid_status == "valid" & alert_status == "alive",
      na.rm = TRUE
    ),
    valid_dead = sum(
      valid_status == "valid" & alert_status == "dead",
      na.rm = TRUE
    ),
    invalid_alive = sum(
      valid_status == "invalid" & alert_status == "alive",
      na.rm = TRUE
    ),
    invalid_dead = sum(
      valid_status == "invalid" & alert_status == "dead",
      na.rm = TRUE
    ),
    alert_source_fosa = sum(alert_source == "fosa", na.rm = TRUE),
    alert_source_ra = sum(alert_source == "ra", na.rm = TRUE),
    alert_source_comm = sum(alert_source == "comm", na.rm = TRUE),
    alert_source_poc = sum(alert_source == "poc", na.rm = TRUE)
  ) |>
  dplyr::reframe(
    .by = c(health_zone, health_area),
    tidyr::complete(
      dplyr::pick(everything()),
      date = seq(min(date), max(date), by = "day")
    )
  ) |>
  mutate(across(all_of(kat_count_cols), \(x) tidyr::replace_na(x, 0L))) |>
  mutate(source_system = "linelist") |>
  mutate(
    date = dplyr::if_else(
      date == as.Date("2029-09-06"),
      as.Date("2026-09-06"),
      date
    )
  )

# Counted alerts must equal alerts with a usable date and area.
stopifnot(
  "Butembo aggregation lost alerts" = sum(
    alert_but$alert_alive + alert_but$alert_dead
  ) ==
    sum(
      !is.na(alert_but_ll$alert_status) &
        !is.na(alert_but_ll$date) &
        !is.na(alert_but_ll$health_area)
    )
)

#* Bind zone registers together ----------------------------------------------

alert_zones <- list(
  musienene = alert_mus,
  katwa = alert_kat,
  butembo = alert_but
)

alerts_raw <- bind_rows(alert_zones)

alerts_clean <- alerts_raw |>
  mutate(
    date = ymd(date),
    week_date = floor_date(date, unit = "week", week_start = 1),
    health_zone = tolower(str_squish(health_zone)),
    health_area = tolower(str_squish(health_area)),
    across(
      c(matches("_(live|dead)$"), starts_with("alert_source_")),
      as.integer
    )
  ) |>
  relocate(week_date, .after = date)

#* Drop unusable rows ----------------------
n_before <- nrow(alerts_clean)
n_no_date <- sum(is.na(alerts_clean$date))
n_no_area <- sum(is.na(alerts_clean$health_area))
alerts_clean <- alerts_clean |>
  filter(!is.na(date), !is.na(health_area))
cli::cli_alert_info(
  "{n_before - nrow(alerts_clean)} of {n_before} rows dropped: {n_no_date} no date, {n_no_area} no area"
)

#* Check duplicate keys --------------------
# A double-entered day would inflate every curve; report, do not distinct().
alert_dupes <- alerts_clean |>
  count(health_zone, health_area, date) |>
  filter(n > 1)
if (nrow(alert_dupes) > 0) {
  cli::cli_alert_warning("{nrow(alert_dupes)} date x zone x area keys repeated")
  print(alert_dupes)
}

#* Export ----------------------------------
saveRDS(
  alert_kat_ll,
  fs::path(
    local_alert_dir,
    glue::glue("alerts_katwa_linelist__{time_write}.rds")
  )
)
saveRDS(
  alerts_clean,
  fs::path(local_alert_dir, glue::glue("alerts_clean__{time_write}.rds"))
)
