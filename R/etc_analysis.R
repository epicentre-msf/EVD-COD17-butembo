# Analyse de la situation du CTE de Butembo
source(here::here("R", "0_global.R"))

# dernière compilation des listes linéaires CTE
etc_compilation <- rio::import(latest_etc_compilation) |>
  as_tibble()

# filter to Butembo sites
etc_bor <- readRDS(latest_etc_compilation_bor) |>
  as_tibble()

but_sites <- c("CT Musienene", "CT UCG", "CTE Kitatumba", "CTE Isiro")


# filter to Butembo
but_etc <- etc_compilation |>
  filter(site %in% but_sites)
but_etc |> count(EVD_status)

but_bor <- etc_bor |> filter(site %in% but_sites)

#* Résumé Admission exits ------------------------

site_levels <- c("CTE Kitatumba", "CT UCG", "CT Musienene", "CTE Isiro")
exit_levels <- c("Actif", "Cured", "Died", "Other")
age_levels <- c("<5", "5-14", "15+", "Total")

etc_adm <- but_etc |>
  filter(date_admission_eff < as.Date("2026-10-12"))

etc_conf <- etc_adm |>
  filter(EVD_status %in% c("Confirmed", "Not a case")) |>
  mutate(
    status = if_else(EVD_status == "Confirmed", "case", "non_case"),
    age_yrs = case_match(
      age_unit,
      "Months" ~ age / 12,
      "Days" ~ age / 365.25,
      .default = age
    ),
    age_group = cut(
      age_yrs,
      c(0, 5, 15, Inf),
      c("<5", "5-14", "15+"),
      right = FALSE
    ),
    type_of_exit = case_when(
      is.na(type_of_exit) ~ "Actif",
      type_of_exit %in% c("Cured", "Died") ~ type_of_exit,
      .default = "Other"
    )
  )

# rows outside the table: statuses other than confirmed or non-case
cat(
  "Admissions before cut-off:",
  nrow(etc_adm),
  "\n",
  "Kept in table:",
  nrow(etc_conf),
  "\n",
  "Dropped (other status):",
  nrow(etc_adm) - nrow(etc_conf),
  "\n"
)

# non-cases have no exit outcome, so they are counted by age group only
exit_counts <- etc_conf |>
  filter(!is.na(age_group)) |>
  mutate(
    age_group = as.character(age_group),
    type_of_exit = if_else(status == "non_case", "Non-case", type_of_exit)
  ) |>
  count(site, age_group, type_of_exit)

exit_counts <- exit_counts |>
  bind_rows(
    exit_counts |>
      summarise(n = sum(n), .by = c(site, type_of_exit)) |>
      mutate(age_group = "Total")
  ) |>
  tidyr::complete(
    site = site_levels,
    age_group = age_levels,
    type_of_exit = c("Non-case", exit_levels),
    fill = list(n = 0)
  ) |>
  mutate(
    conf_total = sum(n[type_of_exit != "Non-case"]),
    total_adm = sum(n),
    .by = c(site, age_group)
  )

etc_exit_df <- exit_counts |>
  mutate(
    cell = case_when(
      type_of_exit == "Non-case" ~
        sprintf("%d (%.0f)", n, if_else(total_adm > 0, 100 * n / total_adm, 0)),
      .default = sprintf(
        "%d (%.0f)",
        n,
        if_else(conf_total > 0, 100 * n / conf_total, 0)
      )
    )
  ) |>
  select(site, age_group, total_adm, type_of_exit, cell) |>
  tidyr::pivot_wider(names_from = type_of_exit, values_from = cell) |>
  mutate(
    site = factor(site, site_levels),
    age_group = factor(age_group, age_levels)
  ) |>
  arrange(site, age_group) |>
  mutate(age_group = as.character(age_group), site = as.character(site)) |>
  select(site, age_group, total_adm, `Non-case`, all_of(exit_levels))

# sites absent from the note have no unknown ages
n_unk_age <- etc_conf |> filter(is.na(age_group)) |> count(site)

etc_exit_tbl <- etc_exit_df |>
  gt::gt(groupname_col = "site") |>
  gt::tab_header(
    title = "Summary of Confirmed cases and non-cases treated in OCP sites",
    subtitle = "Data up to Epi week 41"
  ) |>
  gt::cols_label(
    age_group = "Age",
    total_adm = "Admissions",
    `Non-case` = "Non-cases (% of admissions)"
  ) |>
  gt::tab_spanner(
    label = "Confirmed (% of confirmed)",
    columns = all_of(exit_levels)
  ) |>
  gt::tab_style(
    gt::cell_text(weight = "bold"),
    list(
      gt::cells_body(rows = age_group == "Total"),
      gt::cells_body(columns = total_adm),
      gt::cells_column_labels(),
      gt::cells_row_groups()
    )
  ) |>
  gt::tab_source_note("Total admissions: confirmed cases plus non-cases") |>
  gt::tab_source_note("Other: lost to follow-up or transferred to ETC") |>
  gt::tab_source_note(paste(
    "Unknown age (not shown):",
    paste(n_unk_age$site, n_unk_age$n, collapse = "; ")
  ))

etc_exit_tbl

saveRDS(etc_exit_tbl, fs::path(tables_dir, "etc_exit_tbl.rds"))
gt::gtsave(etc_exit_tbl, fs::path(tables_dir, "etc_exit_tbl.png"))

#* Admissions by site, outcome and age group ------

bar_levels <- c("Non-case", exit_levels)

exit_pal <- c(
  `Non-case` = "#B8E0C8",
  Actif = "#FCD9A8",
  Cured = "#A6CEE3",
  Died = "#F4A9A8",
  Other = "#D9D9D9"
)

site_totals <- exit_counts |>
  filter(age_group == "Total") |>
  distinct(site, total_adm)

exit_bar_df <- exit_counts |>
  filter(age_group != "Total") |>
  left_join(
    site_totals,
    by = join_by(site),
    suffix = c("_age", ""),
    relationship = "many-to-one"
  ) |>
  mutate(
    site = factor(site, rev(site_levels)),
    type_of_exit = factor(type_of_exit, rev(bar_levels)),
    age_group = factor(age_group, rev(age_levels[-4])),
    lab = sprintf("%.0f%%", 100 * n / total_adm)
  )

etc_exit_bar <- ggplot(
  exit_bar_df,
  aes(
    x = n,
    y = site,
    fill = type_of_exit,
    alpha = age_group,
    group = interaction(age_group, type_of_exit)
  )
) +
  geom_col(
    width = 0.7,
    colour = "white",
    linewidth = 0.3,
    position = position_fill()
  ) +
  geom_text(
    aes(label = lab),
    data = \(d) filter(d, n > 0),
    position = position_fill(vjust = 0.5),
    size = 2.5,
    alpha = 1
  ) +
  scale_fill_manual(values = exit_pal, breaks = bar_levels) +
  scale_alpha_manual(
    values = c(`<5` = 0.45, `5-14` = 0.7, `15+` = 1),
    breaks = age_levels[-4]
  ) +
  scale_x_continuous(
    labels = scales::label_percent(),
    expand = expansion(mult = c(0, 0.02))
  ) +
  guides(fill = guide_legend(order = 1, override.aes = list(alpha = 1))) +
  labs(
    x = "% of site admissions",
    y = NULL,
    fill = NULL,
    alpha = "Age group",
    caption = "Admissions up to 11 Oct 2026; unknown age excluded"
  ) +
  epithemes::epitheme_gg() +
  theme(legend.position = "top")

etc_exit_bar

ggsave(
  fs::path(plots_dir, "etc_exit_bar.png"),
  etc_exit_bar,
  width = 15,
  height = 4.5,
  dpi = 300,
  bg = "white"
)


#* OCCUPATION — SYNTHÈSE (reactable) -----------------------------------
# `dispo_*` = capacité installée ; lits confirmés = dispo_total - dispo_SP

occ_all <- occ_dat |>
  # journées entièrement renseignées uniquement
  filter(if_all(
    c(occupied_S, occupied_C, occupied_total, occupation_total),
    ~ !is.na(.x)
  )) |>
  arrange(desc(date)) |>
  transmute(
    site,
    jour = fr_date(date),
    lits_S = dispo_SP,
    n_S = occupied_S,
    tol_S = occupation_SP,
    lits_C = dispo_total - dispo_SP,
    n_C = occupied_C,
    tol_C = occupation_C,
    lits_T = dispo_total,
    n_T = occupied_total,
    tol_T = occupation_total
  )

case_ramp <- c("#ffffff", "#fd7e14")

ramp_style <- function(ramp, domain) {
  pal <- scales::colour_ramp(ramp)
  function(value) {
    if (is.null(value) || is.na(value)) {
      return(list())
    }
    frac <- max(0, min(1, (value - domain[1]) / (domain[2] - domain[1])))
    list(background = pal(frac))
  }
}

# taux d'occupation : dégradé blanc -> orange sur 0-100 %
pct_col <- reactable::colDef(
  name = "%",
  format = reactable::colFormat(percent = TRUE, digits = 0),
  style = ramp_style(case_ramp, c(0, 1))
)

occ_reactable <- reactable::reactable(
  occ_all,
  highlight = TRUE,
  compact = TRUE,
  striped = FALSE,
  pagination = FALSE,
  theme = reactable::reactableTheme(
    style = list(fontSize = "0.82rem"),
    headerStyle = list(fontSize = "0.78rem", fontWeight = 600),
    cellPadding = "4px 6px"
  ),
  defaultColDef = reactable::colDef(align = "center", minWidth = 60),
  columns = list(
    site = reactable::colDef(name = "CTE", align = "left"),
    jour = reactable::colDef(name = "Date", align = "left", sticky = "left"),
    lits_S = reactable::colDef(name = "Lits"),
    n_S = reactable::colDef(name = "N"),
    tol_S = pct_col,
    lits_C = reactable::colDef(name = "Lits"),
    n_C = reactable::colDef(name = "N"),
    tol_C = pct_col,
    lits_T = reactable::colDef(name = "Lits"),
    n_T = reactable::colDef(name = "N"),
    tol_T = pct_col
  ),
  columnGroups = list(
    reactable::colGroup(
      name = "Suspects",
      columns = c("lits_S", "n_S", "tol_S")
    ),
    reactable::colGroup(
      name = "Confirmés",
      columns = c("lits_C", "n_C", "tol_C")
    ),
    reactable::colGroup(name = "Total", columns = c("lits_T", "n_T", "tol_T"))
  )
)

#* FLUX — ADMISSIONS & SORTIES (reactable) -----------------------------
# flux bruts depuis la liste linéaire (l'occupation est un stock, ne sépare
# pas entrées et sorties)

# premier échantillon positif par patient = date de confirmation (approx., car
# date_lab_result pas encore renseignée)
first_pos <- etc_ll |>
  select(
    patient_site_id,
    starts_with("lab_result_"),
    starts_with("date_lab_sample_")
  ) |>
  mutate(
    across(starts_with("lab_result_"), as.character),
    across(starts_with("date_lab_sample_"), as.Date)
  ) |>
  pivot_longer(
    -patient_site_id,
    names_to = c(".value", "test_no"),
    names_pattern = "(.*)_(\\d+)$"
  ) |>
  filter(lab_result == "Positif") |>
  slice_min(date_lab_sample, by = patient_site_id, n = 1, with_ties = FALSE) |>
  select(patient_site_id, first_pos_date = date_lab_sample)

etc_flow <- etc_ll |>
  select(patient_site_id, date_admission_eff, date_exit_eff, type_of_exit) |>
  left_join(first_pos, by = "patient_site_id") |>
  mutate(
    site = "CTE Kitatumba",
    # positif à l'arrivée -> admission confirmée, sinon suspecte
    admitted_confirmed = !is.na(first_pos_date) &
      first_pos_date <= date_admission_eff,
    exit_reason = case_when(
      is.na(date_exit_eff) ~ NA_character_,
      str_detect(
        type_of_exit,
        regex("non cas", ignore_case = TRUE)
      ) ~ "Non cas",
      str_detect(
        type_of_exit,
        regex("gu[ée]ri", ignore_case = TRUE)
      ) ~ "Guéris",
      str_detect(
        type_of_exit,
        regex("d[ée]c[ée]d", ignore_case = TRUE)
      ) ~ "Décédés",
      .default = "Autre"
    )
  )

# fenêtre finissant au dernier jour d'activité de la liste linéaire
flux_asof <- max(
  c(etc_flow$date_admission_eff, etc_flow$date_exit_eff),
  na.rm = TRUE
)

flux_counts <- function(n) {
  since <- flux_asof - n + 1
  adm <- etc_flow$date_admission_eff >= since &
    etc_flow$date_admission_eff <= flux_asof
  ext <- !is.na(etc_flow$date_exit_eff) &
    etc_flow$date_exit_eff >= since &
    etc_flow$date_exit_eff <= flux_asof
  c(
    adm_suspect = sum(adm & !etc_flow$admitted_confirmed),
    adm_confirmed = sum(adm & etc_flow$admitted_confirmed),
    exit_noncase = sum(ext & etc_flow$exit_reason == "Non cas"),
    exit_cured = sum(ext & etc_flow$exit_reason == "Guéris"),
    exit_death = sum(ext & etc_flow$exit_reason == "Décédés")
  )
}

flux_mat <- sapply(c(j1 = 1, j2 = 2, j7 = 7), flux_counts)

flow_tbl <- tibble(
  site = "CTE Kitatumba",
  flux = c("Admissions", "Admissions", "Sorties", "Sorties", "Sorties"),
  categorie = c("Suspects", "Confirmés", "Non cas", "Guéris", "Décédés"),
  key = c(
    "adm_suspect",
    "adm_confirmed",
    "exit_noncase",
    "exit_cured",
    "exit_death"
  )
) |>
  mutate(
    j1 = flux_mat[key, "j1"],
    j2 = flux_mat[key, "j2"],
    j7 = flux_mat[key, "j7"]
  ) |>
  select(-key)

flux_max <- flow_tbl |>
  group_by(site) |>
  summarise(s = sum(j7), .groups = "drop") |>
  pull(s) |>
  max()

count_col <- function(nm) {
  reactable::colDef(
    name = nm,
    aggregate = "sum",
    align = "center",
    style = ramp_style(case_ramp, c(0, flux_max))
  )
}

flow_reactable <- reactable::reactable(
  flow_tbl,
  groupBy = c("site", "flux"),
  defaultExpanded = TRUE,
  highlight = TRUE,
  compact = TRUE,
  striped = FALSE,
  pagination = FALSE,
  theme = reactable::reactableTheme(
    style = list(fontSize = "0.82rem"),
    headerStyle = list(fontSize = "0.78rem", fontWeight = 600),
    cellPadding = "4px 6px"
  ),
  defaultColDef = reactable::colDef(align = "center", minWidth = 60),
  columns = list(
    site = reactable::colDef(name = "CTE", align = "left"),
    flux = reactable::colDef(name = "Flux", align = "left"),
    categorie = reactable::colDef(name = "", align = "left"),
    j1 = count_col("dernières 24h"),
    j2 = count_col("dernières 48h"),
    j7 = count_col("derniers 7j")
  )
)

#* PANNEAU CENTRALISÉ — OCCUPATION + FLUX ------------------------------
site_name <- unique(flow_tbl$site)

etc_panel <- htmltools::browsable(htmltools::div(
  class = "etc-panel",
  style = "font-family: sans-serif; max-width: 900px; border: 1px solid #dee2e6; border-radius: 6px; padding: 14px 18px;",
  htmltools::h2(site_name, style = "margin: 0; font-size: 1.25rem;"),
  htmltools::div(
    paste0("Situation au ", fr_date(flux_asof)),
    style = "color: #6c757d; font-size: 0.85rem; margin-bottom: 14px;"
  ),
  htmltools::h4(
    "Occupation",
    style = "margin: 10px 0 4px; font-size: 0.95rem;"
  ),
  occ_reactable,
  htmltools::h4(
    "Flux — admissions & sorties",
    style = "margin: 18px 0 4px; font-size: 0.95rem;"
  ),
  flow_reactable
))

etc_panel

#* EXPORT POUR LE RAPPORT ----------------------------------------------
# panneau complet (occupation + flux) en PNG pour la version .docx du rapport
save_widget(etc_panel, "butembo_etc_panel.png", selector = ".etc-panel")
