# Script of the overview situation in Butembo
# (combines the former 2_overview.R and butembo_active.R)

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
  gtsummary::modify_caption("**Cas confirmés par zone de santé**") |>
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
  gt::tab_caption(
    gt::md(paste0(
      "**Cas actifs par lieu d'isolement — données au ",
      fr_date(date_report),
      "**"
    ))
  )

active_iso_gt |>
  save_gt("butembo_active_isolation.png")

#? 3. Carte des cas confirmés par aire de santé (adm3) ---------------------------------
# adm2_isolation / adm3_isolation sont pré-découpés depuis isolation_site_id (voir 1_prep_data.R).
cases_sf <- adm3 |>
  left_join(
    pos_data_clean |>
      count(adm3_isolation, name = "n_conf"),
    by = join_by("adm3_name" == "adm3_isolation")
  )

# part des cas confirmés avec une aire de santé d'isolement connue (adm3)
n_total <- nrow(pos_data_clean)
n_located <- sum(!is.na(pos_data_clean$adm3_isolation))
pct_located <- round(100 * n_located / n_total)

map_caption <- glue::glue(
  "{n_located} ({pct_located}%) des {n_total} cas ont une information sur l'aire de santé d'isolement"
)

# un point intérieur par aire de santé avec des cas (garanti dans le polygone)
cases_pts <- cases_sf |>
  filter(!is.na(n_conf)) |>
  st_point_on_surface()

tm_butembo_conf <- tm_basemap_epi() +
  # toutes les limites d'aires de santé (y compris sans cas)
  tm_shape(adm3, bbox = st_bbox(adm3)) +
  tm_borders(col = "grey60", lwd = 1) +
  # limites de zones de santé au-dessus
  tm_shape(adm2) +
  tm_borders(col = "grey20", lwd = 1.3) +
  # cercles proportionnels aux centroïdes
  tm_shape(cases_pts) +
  tm_symbols(
    size = "n_conf",
    size.scale = tm_scale_continuous(
      values.scale = 2,
      values.range = c(0.4, 1)
    ),
    size.legend = tm_legend(title = "N cas"),
    fill = "#bb3e03",
    fill_alpha = 0.8,
    col = "white",
    lwd = 0.5
  ) +
  tm_text("n_conf", size = 0.6, col = "white") +
  tm_theme_epi(
    credits = map_caption,
    date = date_report,
    scalebar_breaks = c(0, 5, 10)
  )

tmap_save(
  tm_butembo_conf,
  fs::path(out_dir, "butembo_map_confirmed_reporting.png"),
  height = 8,
  width = 8,
  dpi = 300
)

#? 4. Létalité (CFR) — cohorte totale et cohorte complète ------------------------------

# Dernière date de début des symptômes jusqu'à laquelle aucun cas n'est encore
# "Actif" : tous les cas ont atteint une issue terminale (Guéri, Décédé ou Abandon),
last_complete_onset <- pos_data_clean |>
  filter(!is.na(date_symptom_onset)) |>
  summarise(
    .by = date_symptom_onset,
    n_active = sum(type_of_exit == "Actif", na.rm = TRUE)
  ) |>
  arrange(date_symptom_onset) |>
  filter(cumall(n_active == 0)) |> # série de tête sans cas encore actif
  slice_max(date_symptom_onset, n = 1) |>
  pull(date_symptom_onset)

# Aide : décès / cas résolus (Guéri ou Décédé)
cfr_summary <- function(df) {
  df |>
    summarise(
      n_death = sum(type_of_exit == "Décédé", na.rm = TRUE),
      n_resolved = sum(type_of_exit %in% c("Guéri", "Décédé"))
    )
}

cfr_tbl <- bind_rows(
  # Cohorte totale : tous les cas résolus, toute date de début
  pos_data_clean |>
    cfr_summary() |>
    mutate(cohort = "Tous les cas"),
  # Cohorte complète : début des symptômes au plus tard à la dernière date résolue
  pos_data_clean |>
    filter(date_symptom_onset <= last_complete_onset) |>
    cfr_summary() |>
    mutate(
      cohort = paste0(
        "Cohorte complète (≤ ",
        format(last_complete_onset, "%d/%m/%Y"),
        ")"
      )
    )
) |>
  mutate(cfr = n_death / n_resolved) |>
  select(cohort, n_death, n_resolved, cfr)

cfr_gt <- cfr_tbl |>
  gt::gt() |>
  gt::cols_label(
    cohort = "Cohorte",
    n_death = "Décès",
    n_resolved = "Cas résolus",
    cfr = "Létalité (CFR)"
  ) |>
  gt::fmt_percent(cfr, decimals = 1) |>
  gt::tab_caption(
    gt::md(paste0(
      "**Létalité — cas confirmés — données au ",
      fr_date(date_report),
      "**"
    ))
  ) |>
  gt::tab_footnote(
    "Létalité = décès / cas résolus (Guéri ou Décédé), excluant les cas actifs."
  )

cfr_gt |>
  save_gt("butembo_cfr.png")
