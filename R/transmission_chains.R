# Reconstruct Butembo transmission chains from the cleaned linelist.
# Reads the latest local linelist from 1_prep_data.R.
# Writes chain tables and the node/edge inputs for epishiny.contacts.

#* TODO ------------------------------------

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))
library(epicontacts)
library(shiny)
library(epishiny.contacts)

#* Import data -----------------------------
latest_ll <- fs::dir_ls(local_ll_dir, regexp = "_linelist__.*\\.rds$") |>
  max()
ll <- readRDS(latest_ll)
cli::cli_alert_info("{fs::path_file(latest_ll)}: {nrow(ll)} rows")

latest_events <- fs::dir_ls(
  local_transmission_dir,
  regexp = "_transmission_events__.*\\.rds$"
) |>
  max()
transmission_events <- readRDS(latest_events)
cli::cli_alert_info(
  "{fs::path_file(latest_events)}: {nrow(transmission_events)} events"
)

#* Same health area and zone --------------
# one onset place per pid; a pid shared by several patients cannot be placed
place_lookup <- ll |>
  filter(!is.na(pid)) |>
  mutate(pid = as.character(pid)) |>
  summarise(
    n_rows = n(),
    n_areas = n_distinct(adm3_name__onset, na.rm = TRUE),
    n_zones = n_distinct(adm2_name__onset, na.rm = TRUE),
    adm3_onset = dplyr::first(na.omit(adm3_name__onset)),
    adm2_onset = dplyr::first(na.omit(adm2_name__onset)),
    .by = pid
  ) |>
  mutate(shared = n_rows > 1 & (n_areas > 1 | n_zones > 1))
cli::cli_alert_info(
  "{sum(place_lookup$shared)} pid dropped from the place lookup: several rows, \
   different onset places"
)
place_lookup <- place_lookup |>
  filter(!shared) |>
  select(pid, adm3_onset, adm2_onset)

# one event per infectee, set 1 first, so each case counts once
n_events <- nrow(transmission_events)
events_one <- transmission_events |>
  filter(!is.na(from_pid), !is.na(to_pid)) |>
  mutate(across(c(from_pid, to_pid), as.character)) |>
  arrange(to_pid, transmission_set) |>
  slice(1, .by = to_pid)
cli::cli_alert_info(
  "{n_events - nrow(events_one)} of {n_events} events dropped: no from/to pid, \
   or a second event for the same infectee"
)

n_before <- nrow(events_one)
events_place <- events_one |>
  left_join(
    rename(place_lookup, from_area = adm3_onset, from_zone = adm2_onset),
    by = join_by(from_pid == pid),
    relationship = "many-to-one"
  ) |>
  left_join(
    rename(place_lookup, to_area = adm3_onset, to_zone = adm2_onset),
    by = join_by(to_pid == pid),
    relationship = "many-to-one"
  ) |>
  mutate(
    area_link = case_when(
      is.na(from_area) | is.na(to_area) ~ "Unknown",
      from_area == to_area ~ "Same",
      .default = "Different"
    ),
    zone_link = case_when(
      is.na(from_zone) | is.na(to_zone) ~ "Unknown",
      from_zone == to_zone ~ "Same",
      .default = "Different"
    )
  )
stopifnot("place join changed N" = nrow(events_place) == n_before)

# % over events with both places known; unknown ones are counted, not hidden
same_place <- events_place |>
  tidyr::pivot_longer(
    c(area_link, zone_link),
    names_to = "admin_level",
    values_to = "link"
  ) |>
  summarise(
    n_links = n(),
    n_place_unknown = sum(link == "Unknown"),
    n_place_known = sum(link != "Unknown"),
    n_same_place = sum(link == "Same"),
    pct_same_place = round(100 * n_same_place / n_place_known, 1),
    .by = admin_level
  ) |>
  mutate(
    admin_level = unname(c(
      area_link = "Health area",
      zone_link = "Health zone"
    )[
      admin_level
    ])
  )
print(same_place)

#* 10th outbreak------------------------

evd_10_data <- readRDS(fs::path(local_10th_dir, "mll_tc.rds"))

evd10_ll <- evd_10_data$mll
evd10_tc <- evd_10_data$tc

# ll lookup
evd_10_place_lookup <- evd10_ll |>
  filter(!is.na(vhf_code)) |>
  mutate(pid = as.character(vhf_code)) |>
  summarise(
    n_rows = n(),
    n_areas = n_distinct(adm3, na.rm = TRUE),
    n_zones = n_distinct(adm2, na.rm = TRUE),
    adm3_onset = dplyr::first(na.omit(adm3)),
    adm2_onset = dplyr::first(na.omit(adm2)),
    .by = pid
  ) |>
  mutate(shared = n_rows > 1 & (n_areas > 1 | n_zones > 1)) |>
  filter(!shared) |>
  select(pid, adm3_onset, adm2_onset)

# one event per infectee, set 1 first, so each case counts once
events_evd10_one <- evd10_tc |>
  filter(!is.na(from), !is.na(to)) |>
  mutate(across(c(from, to), as.character)) |>
  arrange(to) |>
  slice(1, .by = to)

events_place_evd10 <- events_evd10_one |>
  left_join(
    rename(evd_10_place_lookup, from_area = adm3_onset, from_zone = adm2_onset),
    by = join_by(from == pid),
    relationship = "many-to-one"
  ) |>
  left_join(
    rename(evd_10_place_lookup, to_area = adm3_onset, to_zone = adm2_onset),
    by = join_by(to == pid),
    relationship = "many-to-one"
  ) |>
  mutate(
    area_link = case_when(
      is.na(from_area) | is.na(to_area) ~ "Unknown",
      from_area == to_area ~ "Same",
      .default = "Different"
    ),
    zone_link = case_when(
      is.na(from_zone) | is.na(to_zone) ~ "Unknown",
      from_zone == to_zone ~ "Same",
      .default = "Different"
    )
  )

# % over events with both places known; unknown ones are counted, not hidden
same_place_evd10 <- events_place_evd10 |>
  tidyr::pivot_longer(
    c(area_link, zone_link),
    names_to = "admin_level",
    values_to = "link"
  ) |>
  summarise(
    n_links = n(),
    n_place_unknown = sum(link == "Unknown"),
    n_place_known = sum(link != "Unknown"),
    n_same_place = sum(link == "Same"),
    pct_same_place = round(100 * n_same_place / n_place_known, 1),
    .by = admin_level
  ) |>
  mutate(
    admin_level = unname(c(
      area_link = "Health area",
      zone_link = "Health zone"
    )[
      admin_level
    ])
  )
print(same_place_evd10)

#* Final Gt table ------------------------
same_place_gt <- dplyr::bind_rows(
  mutate(same_place, outbreak = "17th outbreak (Butembo)"),
  mutate(same_place_evd10, outbreak = "10th outbreak")
) |>
  select(
    outbreak,
    admin_level,
    n_links,
    n_place_known,
    n_same_place,
    pct_same_place
  ) |>
  gt::gt(groupname_col = "outbreak", row_group_as_column = FALSE) |>
  gt::fmt_number(columns = pct_same_place, decimals = 1) |>
  gt::cols_merge(
    columns = c(n_same_place, pct_same_place),
    pattern = "{1} ({2}%)"
  ) |>
  gt::cols_label(
    admin_level = "Level",
    n_links = "N links",
    n_place_known = "N links with complete geo",
    n_same_place = "N links same geo (%)"
  ) |>
  gt::tab_header(
    title = "Transmission within the same health area and zone",
  ) |>
  gt::cols_align(align = "center", columns = gt::everything()) |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  )
same_place_gt

saveRDS(same_place_gt, fs::path(tables_dir, "same_place_gt.rds"))

#* HF catchment vs patient residence ------
latest_hf <- fs::dir_ls(local_hf_dir, regexp = "_hf_visits__.*\\.rds$") |>
  max()
hf_visits <- readRDS(latest_hf)
cli::cli_alert_info("{fs::path_file(latest_hf)}: {nrow(hf_visits)} visits")

# isolation sites have no catchment in the sense asked here
n_visits <- nrow(hf_visits)
hf_visited <- hf_visits |>
  filter(visit_type == "visited") |>
  mutate(
    # placeholder labels from the export are not places
    across(
      c(hf_as, hf_zs),
      \(x) if_else(str_detect(x, "non définie"), NA_character_, x)
    )
  )
cli::cli_alert_info(
  "{n_visits - nrow(hf_visited)} of {n_visits} visits dropped: isolation rows"
)

# a patient returning to one facility must not count twice
n_visited <- nrow(hf_visited)
hf_visited <- hf_visited |>
  distinct(unique_id, hf_name, hf_as, hf_zs)
cli::cli_alert_info(
  "{n_visited - nrow(hf_visited)} of {n_visited} visits dropped: repeat \
   patient-facility pairs"
)

# unique_id repeats in the linelist; conflicting residences cannot be placed
res_lookup <- ll |>
  summarise(
    n_areas = n_distinct(adm3_name__res, na.rm = TRUE),
    n_zones = n_distinct(adm2_name__res, na.rm = TRUE),
    res_as = dplyr::first(na.omit(adm3_name__res)),
    res_zs = dplyr::first(na.omit(adm2_name__res)),
    .by = unique_id
  ) |>
  mutate(shared = n_areas > 1 | n_zones > 1)
cli::cli_alert_info(
  "{sum(res_lookup$shared)} unique_id dropped from the residence lookup: \
   several residences"
)
res_lookup <- res_lookup |>
  filter(!shared) |>
  select(unique_id, res_as, res_zs)

n_before <- nrow(hf_visited)
hf_place <- hf_visited |>
  left_join(
    res_lookup,
    by = join_by(unique_id),
    relationship = "many-to-one"
  ) |>
  mutate(
    # area names can repeat across zones, so the zone must match too
    area_link = case_when(
      is.na(hf_as) | is.na(res_as) | is.na(hf_zs) | is.na(res_zs) ~ "Unknown",
      hf_as == res_as & hf_zs == res_zs ~ "Same",
      .default = "Different"
    ),
    zone_link = case_when(
      is.na(hf_zs) | is.na(res_zs) ~ "Unknown",
      hf_zs == res_zs ~ "Same",
      .default = "Different"
    )
  )
stopifnot("residence join changed N" = nrow(hf_place) == n_before)

hf_catchment <- hf_place |>
  tidyr::pivot_longer(
    c(area_link, zone_link),
    names_to = "admin_level",
    values_to = "link"
  ) |>
  summarise(
    n_pairs = n(),
    n_geo_known = sum(link != "Unknown"),
    n_same = sum(link == "Same"),
    pct_same = round(100 * n_same / n_geo_known, 1),
    .by = admin_level
  ) |>
  mutate(
    admin_level = unname(c(
      area_link = "Health area",
      zone_link = "Health zone"
    )[
      admin_level
    ])
  )
print(hf_catchment)

hf_catchment_gt <- hf_catchment |>
  gt::gt() |>
  gt::fmt_number(columns = pct_same, decimals = 1) |>
  gt::cols_merge(columns = c(n_same, pct_same), pattern = "{1} ({2}%)") |>
  gt::cols_label(
    admin_level = "Level",
    n_pairs = "N patient-facility pairs",
    n_geo_known = "N pairs with complete geo",
    n_same = "N pairs same (%)"
  ) |>
  gt::tab_header(
    title = "Visited facility in cases' health area and zone",
    subtitle = "Facility area and zone vs patient residence, non-isolation, one row per patient-facility pair"
  ) |>
  gt::cols_align(align = "center", columns = gt::everything())
hf_catchment_gt

saveRDS(hf_catchment_gt, fs::path(tables_dir, "hf_catchment_gt.rds"))

gt::gtsave(same_place_gt, fs::path(local_tables_dir, "same_place_gt.png"))
gt::gtsave(hf_catchment_gt, fs::path(local_tables_dir, "hf_catchment_gt.png"))

#* Summary statistics ----------------------
# pairs with known infector and infectee, one per infectee
cli::cli_alert_info("{nrow(events_one)} transmission pairs reconstructed")

events_one |>
  mutate(
    transmission_type = case_match(
      transmission_type,
      c(NA, "Inconnu") ~ "Unknown",
      .default = transmission_type
    )
  ) |>
  count(transmission_type, sort = TRUE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  janitor::adorn_totals()

# cases with no named structure count as 0 visited
hf_visits |>
  filter(visit_type == "visited", !is.na(hf_name)) |>
  summarise(n_structures = n_distinct(hf_name), .by = unique_id) |>
  right_join(
    distinct(ll, unique_id),
    by = join_by(unique_id),
    relationship = "one-to-one"
  ) |>
  summarise(mean_structures = mean(coalesce(n_structures, 0L)))

# per visit; los is NA when dates are missing or inverted
hf_visits |>
  filter(visit_type == "visited") |>
  summarise(
    n_visits = n(),
    n_los_missing = sum(is.na(los)),
    mean_los = mean(los, na.rm = TRUE)
  )

# isolated_here keeps isolation recorded under a structure also visited
hf_visits |>
  summarise(
    isolated = any(visit_type == "isolation" | isolated_here),
    .by = unique_id
  ) |>
  right_join(
    distinct(ll, unique_id),
    by = join_by(unique_id),
    relationship = "one-to-one"
  ) |>
  summarise(
    n_cases = n(),
    n_isolated = sum(isolated, na.rm = TRUE),
    pct_isolated = round(100 * n_isolated / n_cases, 1)
  )
