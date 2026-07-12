# Analyse de la situation du CTE de Butembo
source(here::here("R", "0_global.R"))

# dernier export de la liste linéaire du CTE (Kitatumba)
etc_ll <- rpxl::rp_xlsb(etc_ll_path, password = "ebolaExport", sheet = 1) |>
  as_tibble()

# données d'occupation
occ_dat <- rpxl::rp_xlsb(
  etc_ll_path,
  password = "ebolaExport",
  sheet = "occupancy"
) |>
  as_tibble() |>
  select(-contains("_P"), -contains("column")) |>

  # ! this to adress when multiple sites
  mutate(site = "CTE Kitatumba")

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
