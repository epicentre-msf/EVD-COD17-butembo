# Overview of the situation in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)

#? 1. Statut des cas par zone de santé ---------------------------------------------------
# Statut du patient (Actif / Guéri / Abandon / Décédé) et effectif par zone de santé.
positives_summary <- pos_data_clean |>
  select(adm2_comptabilisation, type_of_exit) |>
  gtsummary::tbl_summary(
    by = adm2_comptabilisation,
    label = list(
      type_of_exit ~ "Statut du patient"
    ),
    missing_text = "Inconnu"
  ) |>
  gtsummary::modify_header(label ~ "") |>
  gtsummary::modify_spanning_header(
    gtsummary::all_stat_cols() ~ "**Zone de santé**"
  ) |>
  gtsummary::modify_source_note(paste0(
    "Données au ",
    fr_date(date_report)
  ))

positives_summary |>
  gtsummary::as_gt() |>
  save_gt("butembo_pos_summary.png")

#? 2. Cas actifs par lieu d'isolement ---------------------------------------------------

active_iso <- pos_data_clean |>
  filter(type_of_exit == "Actif") |>
  count(adm2_isolation, isolation_site_id, name = "n_actif") |>
  mutate(across(
    c(adm2_isolation, isolation_site_id),
    ~ tidyr::replace_na(.x, "Inconnu")
  )) |>
  mutate(
    # sentence case, but keep the "HGR" prefix uppercase
    isolation_site_id = str_to_sentence(isolation_site_id),
    isolation_site_id = str_replace(
      isolation_site_id,
      regex("^hgr", ignore_case = TRUE),
      "HGR"
    ),
    # shorten the long Graben clinic name
    isolation_site_id = if_else(
      str_detect(isolation_site_id, regex("graben", ignore_case = TRUE)),
      "U.C.G",
      isolation_site_id
    )
  ) |>
  arrange(adm2_isolation, desc(n_actif))

active_iso_gt <- active_iso |>
  gt::gt(
    rowname_col = "isolation_site_id",
    groupname_col = "adm2_isolation"
  ) |>
  gt::tab_stubhead(label = "Structure d'isolement") |>
  gt::cols_label(
    n_actif = "Cas actifs"
  ) |>
  gt::summary_rows(
    groups = gt::everything(),
    columns = n_actif,
    fns = list("Sous-total" ~ sum(.)),
    fmt = ~ gt::fmt_number(., decimals = 0)
  ) |>
  gt::grand_summary_rows(
    columns = n_actif,
    fns = list(Total ~ sum(.)),
    fmt = ~ gt::fmt_number(., decimals = 0)
  ) |>
  # zone de santé (adm2) group labels in bold
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  ) |>
  # structure names left-aligned
  gt::tab_style(
    style = gt::cell_text(align = "left"),
    locations = gt::cells_stub()
  ) |>
  gt::tab_source_note(
    gt::md(paste0("Données au ", fr_date(date_report)))
  )

active_iso_gt |>
  save_gt("butembo_active_isolation.png")

# Carte des cas confirmés par aire de santé : déplacée dans R/4_place.R
# Létalité (CFR) : déplacée dans R/9_cfr.R

#? 3. Cas par zone de santé (adm2) et aire de santé (adm3) ---------------------
# Distribution géographique par résidence. Les cas résidant hors des zones de
# santé suivies (Butembo, Katwa, Musienene) sont regroupés en "Hors-zone".

local_hz <- c("Butembo", "Katwa", "Musienene")

# classification par résidence, réutilisée pour les effectifs et la répartition
place_base <- pos_data_clean |>
  mutate(
    adm2_grp = case_when(
      adm2_name %in% local_hz ~ adm2_name,
      is.na(adm2_name) ~ "Inconnu",
      .default = "Hors-zone"
    ),
    adm3_grp = case_when(
      adm2_name %in% local_hz ~ tidyr::replace_na(adm3_name, "Inconnu"),
      is.na(adm2_name) ~ "Inconnu",
      .default = "Hors-zone"
    )
  )

# alertes : nouvelles alertes au dernier point de situation de chaque zone
alert_latest <- readRDS(latest_alert_clean) |>
  summarise(
    .by = c(sitrep_date, adm2_name, adm3_name),
    n_alert = sum(alert_new, na.rm = TRUE)
  ) |>
  slice_max(sitrep_date, n = 1, by = adm2_name) |>
  select(adm2_name, adm3_name, n_alert)

# contacts à suivre : dernier point de situation propre à chaque zone de santé
contact_latest <- readRDS(latest_contact_clean) |>
  summarise(
    .by = c(sitrep_date, adm2_name, adm3_name),
    contact_to_follow = sum(contact_to_follow, na.rm = TRUE),
    contact_seen = sum(contact_seen, na.rm = TRUE)
  ) |>
  slice_max(sitrep_date, n = 1, by = adm2_name) |>
  mutate(
    follow_rate = if_else(
      contact_to_follow > 0,
      round(100 * contact_seen / contact_to_follow, 1),
      NA_real_
    )
  ) |>
  select(
    adm2_name,
    adm3_name,
    contact_to_follow,
    follow_rate,
    date_last_contact = sitrep_date
  )

cases_by_place <- place_base |>
  summarise(
    .by = c(adm2_grp, adm3_grp),
    n_case = n(),
    date_last_onset = max(date_symptom_onset, na.rm = TRUE),
    date_last_notif = max(date_notification, na.rm = TRUE)
  ) |>
  # une colonne d'effectif par statut d'infection (Incertaine / Importée / Locale)
  left_join(
    place_base |>
      count(adm2_grp, adm3_grp, infection_butembo) |>
      tidyr::pivot_wider(
        names_from = infection_butembo,
        values_from = n,
        values_fill = 0
      ),
    by = c("adm2_grp", "adm3_grp")
  ) |>
  # alertes et suivi des contacts (rattachés par aire de santé)
  left_join(
    alert_latest,
    by = join_by("adm2_grp" == "adm2_name", "adm3_grp" == "adm3_name")
  ) |>
  left_join(
    contact_latest,
    by = join_by("adm2_grp" == "adm2_name", "adm3_grp" == "adm3_name")
  ) |>
  mutate(contact_to_follow = tidyr::replace_na(contact_to_follow, 0)) |>
  relocate(all_of(c("Locale", "Importée", "Incertaine")), .after = n_case) |>
  relocate(date_last_onset, .after = last_col()) |>
  relocate(date_last_notif, .after = last_col()) |>
  relocate(date_last_contact, .after = last_col()) |>
  mutate(
    adm2_grp = forcats::fct_relevel(adm2_grp, local_hz, "Hors-zone", "Inconnu")
  ) |>
  arrange(adm2_grp, adm3_grp == "Inconnu", desc(n_case))

# domaines des dégradés calculés sur les seules zones locales
# (Hors-zone / Inconnu exclus pour ne pas écraser l'échelle de couleur)
local_place <- cases_by_place |>
  filter(adm2_grp %in% local_hz)
dom_case <- range(local_place$n_case, na.rm = TRUE)
dom_locale <- range(local_place$Locale, na.rm = TRUE)
dom_contact <- range(local_place$contact_to_follow, na.rm = TRUE)

# répartition d'origine (footnote) : les hors-zone / inconnu ne sont pas
# présentés, ce décompte en rend compte au bas de chaque tableau
n_total <- nrow(place_base)
n_valid <- sum(
  place_base$adm2_name %in% local_hz & !is.na(place_base$adm3_name)
)
n_horszone <- sum(
  !is.na(place_base$adm2_name) & !(place_base$adm2_name %in% local_hz)
)
n_unknown <- n_total - n_valid - n_horszone

place_footnote <- paste0(
  n_valid,
  "/",
  n_total,
  " (",
  round(100 * n_valid / n_total),
  " %) des cas ont une aire de santé valide et un début des symptômes ",
  "dans la zone · ",
  n_horszone,
  " arrivés malades de l'extérieur (hors zone) · ",
  n_unknown,
  " d'origine inconnue"
)

# construit un tableau par zone de santé ; `colored = FALSE` pour "Autres"
place_gt <- function(dat, title, colored = TRUE) {
  # dernier point de situation propre à la zone (alertes / contacts)
  date_zone <- max(dat$date_last_contact, na.rm = TRUE)
  g <- dat |>
    select(-adm2_grp, -date_last_contact) |>
    gt::gt(rowname_col = "adm3_grp") |>
    gt::tab_header(title = paste("Zone de santé", title)) |>
    gt::tab_stubhead(label = "Aire de santé") |>
    gt::tab_spanner(
      label = "Infection",
      columns = c("Incertaine", "Importée", "Locale")
    ) |>
    gt::cols_label(
      n_case = "N cas",
      n_alert = "Alertes",
      contact_to_follow = "Contacts à suivre",
      follow_rate = "Taux de suivi (%)",
      date_last_onset = "Dernier début des symptômes",
      date_last_notif = "Dernière notification"
    ) |>
    gt::fmt_date(
      columns = c(date_last_onset, date_last_notif),
      date_style = "day_m_year",
      locale = "fr"
    ) |>
    gt::sub_missing(missing_text = "—") |>
    # aire de santé names left-aligned
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = gt::cells_stub()
    ) |>
    gt::tab_source_note(
      gt::md(paste0("Données au ", fr_date(date_report)))
    ) |>
    gt::tab_source_note(
      gt::md(paste0(
        "Alertes et contacts : données au ",
        fr_date(date_zone)
      ))
    ) |>
    gt::tab_source_note(
      gt::md(paste0(
        "Seules les aires de santé ayant notifié au moins un cas sont ",
        "présentées ; des aires sans cas peuvent avoir des alertes ou des ",
        "contacts en cours de suivi."
      ))
    ) |>
    gt::tab_source_note(gt::md(place_footnote))
  if (colored) {
    g <- g |>
      gt::data_color(
        columns = n_case,
        palette = c("#fff7ec", "#bb3e03"),
        domain = dom_case
      ) |>
      gt::data_color(
        columns = Locale,
        palette = c("#e5f5e0", "#00441b"),
        domain = dom_locale
      ) |>
      gt::data_color(
        columns = contact_to_follow,
        palette = c("#f7fbff", "#3a7ca5"),
        domain = dom_contact
      ) |>
      # taux de suivi : rouge → vert en tons pastel (0–100 %)
      gt::data_color(
        columns = follow_rate,
        palette = c("#e28f8f", "#f7e6a1", "#9fce9f"),
        domain = c(0, 100),
        na_color = "white"
      )
  }
  g
}

gt_butembo <- place_gt(filter(cases_by_place, adm2_grp == "Butembo"), "Butembo")
gt_katwa <- place_gt(filter(cases_by_place, adm2_grp == "Katwa"), "Katwa")
gt_musienene <- place_gt(
  filter(cases_by_place, adm2_grp == "Musienene"),
  "Musienene"
)

# aperçu avant sauvegarde
gt_butembo
gt_katwa
gt_musienene

gt_butembo |> save_gt("butembo_cases_by_place_butembo.png")
gt_katwa |> save_gt("butembo_cases_by_place_katwa.png")
gt_musienene |> save_gt("butembo_cases_by_place_musienene.png")
