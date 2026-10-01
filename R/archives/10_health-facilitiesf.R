# ! Script of the patient HF travels (parcours de soins avant isolement)

source(here::here("R", "0_global.R"))
# shared with the dashboard's Health facilities tab
source(here::here("R", "butembo_dashboard", "fn_facilities.R"))
pos_data_clean <- readRDS(latest_narr_ll_clean)
date_report <- clean_file_date(latest_narr_ll_clean) # export date stamp

# one row per recorded HF visit
hf_visits <- pos_data_clean |>
  select(
    patient_name,
    contains("HF_name_visited"),
    contains("date_start_HF_visited"),
    contains("date_end_HF_visited")
  ) |>
  # coerce to character so all visit columns stack
  mutate(across(-patient_name, as.character)) |>
  pivot_longer(
    cols = -patient_name,
    names_to = c(".value", "visit"),
    names_pattern = "(.*?)(\\d+)$",
    names_transform = list(visit = as.integer)
  ) |>
  rename_with(\(x) str_remove(x, "_$")) |> # strip any trailing underscore
  mutate(across(where(is.character), \(x) na_if(str_squish(x), ""))) |>
  filter(!is.na(HF_name_visited)) |>
  rename(hf_name = HF_name_visited) |>
  # split "site | aire de santé | zone de santé"; Graben clinic labelled UCG
  mutate(
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
    hf_name = if_else(
      str_detect(hf_name, regex("graben|clinique", ignore_case = TRUE)),
      "UCG",
      hf_name
    )
  ) |>
  mutate(across(c(date_start_HF_visited, date_end_HF_visited), as.Date)) |>
  # data correction (raw export): 2030-05-26 entered instead of 2026-05-30
  mutate(across(
    c(date_start_HF_visited, date_end_HF_visited),
    \(x) {
      if_else(
        str_detect(patient_name, regex("donatien", ignore_case = TRUE)) &
          x == as.Date("2030-05-26"),
        as.Date("2026-05-30"),
        x
      )
    }
  )) |>
  # length of stay per structure (days); negative = data-entry error -> NA
  mutate(
    los = as.numeric(date_end_HF_visited - date_start_HF_visited),
    los = if_else(los < 0, NA_real_, los)
  )

# Distinct structures visited per case.
n_structures <- hf_visits |>
  summarise(.by = patient_name, n_structures = n_distinct(hf_name))

#* Distributions ------------------------------------------------------
# discrete counts, shown as integer-binned histograms
dist_data <- bind_rows(
  tibble::tibble(
    metric = "Nombre de structures visitées (par cas)",
    value = n_structures$n_structures
  ),
  tibble::tibble(
    metric = "Durée de séjour par structure (jours)",
    value = hf_visits$los
  )
) |>
  filter(!is.na(value)) |>
  mutate(
    metric = factor(
      metric,
      levels = c(
        "Nombre de structures visitées (par cas)",
        "Durée de séjour par structure (jours)"
      )
    )
  )

butembo_hf_dist <- dist_data |>
  ggplot(aes(x = value)) +
  # centre each bin on its integer value
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    colour = "white",
    fill = "#3a7ca5",
    alpha = 0.65
  ) +
  facet_wrap(~metric, ncol = 1, scales = "free") +
  scale_x_continuous(
    breaks = scales::breaks_width(1),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = NULL,
    y = "Effectif",
    caption = paste0("Données au ", fr_date(date_report))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey85",
      linewidth = 0.2,
      linetype = "62"
    ),
    strip.text = element_text(face = "bold", hjust = 0, size = 11),
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_hf_dist

ggsave(
  fs::path(plots_dir, "butembo_hf_visits_distribution.png"),
  butembo_hf_dist,
  height = 8,
  width = 7,
  dpi = 300,
  bg = "white"
)

#* Structures ayant vu le plus de cas — 7 / 14 / 21 j -----------------
# Parcours de soins : une structure "voit" un cas s'il y a séjourné (date de
# début de visite) dans la fenêtre, ancrée sur la date du rapport et cumulative
# (7 j inclus dans 14 j inclus dans 21 j). CTE exclus : ils reçoivent les cas
# confirmés, hors parcours de soins pré-isolement.
hf_recent <- hf_top_structures(
  hf_visits,
  anchor = date_report,
  id_col = "patient_name"
)

# dégradé de couleur partagé par les trois colonnes de comptage
case_ramp <- c("#ffffff", "#fd7e14")
dom_seen <- c(0, max(hf_recent$j21))

hf_theme <- reactable::reactableTheme(
  style = list(fontSize = "0.82rem"),
  headerStyle = list(fontSize = "0.78rem", fontWeight = 600),
  cellPadding = "4px 6px"
)

count_col <- function(nm) {
  reactable::colDef(name = nm, style = ramp_style(case_ramp, dom_seen))
}

hf_recent_reactable <- reactable::reactable(
  hf_recent,
  highlight = TRUE,
  compact = TRUE,
  striped = FALSE,
  pagination = FALSE,
  theme = hf_theme,
  defaultColDef = reactable::colDef(align = "center", minWidth = 60),
  columns = list(
    hf_name = reactable::colDef(
      name = "Structure",
      align = "left",
      sticky = "left",
      minWidth = 170
    ),
    hf_as = reactable::colDef(name = "Aire de santé", align = "left"),
    hf_zs = reactable::colDef(name = "Zone de santé", align = "left"),
    total = reactable::colDef(
      name = "Total",
      style = list(fontWeight = 600),
      minWidth = 70
    ),
    j7 = count_col("7 j"),
    j14 = count_col("14 j"),
    j21 = count_col("21 j")
  )
)

hf_recent_panel <- htmltools::browsable(htmltools::div(
  class = "hf-recent",
  style = "font-family: sans-serif; max-width: 760px; border: 1px solid #dee2e6; border-radius: 6px; padding: 14px 18px;",
  htmltools::h2(
    "Structures ayant vu le plus de cas",
    style = "margin: 0; font-size: 1.25rem;"
  ),
  htmltools::div(
    paste0(
      "Cas vus dans les 7 / 14 / 21 derniers jours — données au ",
      fr_date(date_report)
    ),
    style = "color: #6c757d; font-size: 0.85rem; margin-bottom: 14px;"
  ),
  hf_recent_reactable,
  htmltools::div(
    "HGR Kitatumba, HGR Katwa et HGR Matanda sont exclus : ce sont des centres de transit/traitement (CT/CTE).",
    style = "color: #6c757d; font-size: 0.78rem; margin-top: 10px;"
  )
))

hf_recent_panel

hf_recent_panel |>
  save_widget("butembo_hf_top_structures.png", selector = ".hf-recent")

#* Cartographie des structures récentes -------------------------------
# Rapproche chaque structure du tableau de la couche FOSA géolocalisée (hf) :
# correspondance approximative (Jaro-Winkler) au sein de la même aire de santé,
# complétée par une table de correspondance manuelle pour les cas particuliers.
hf_ref <- hf_prepare_ref(hf)

hf_recent_geo <- hf_recent |>
  mutate(ref_row = purrr::map2_int(hf_name, hf_as, \(x, y) hf_match_idx(x, y, hf_ref)))

n_unmapped <- sum(is.na(hf_recent_geo$ref_row))

hf_points <- hf_recent_geo |>
  filter(!is.na(ref_row)) |>
  (\(df) {
    st_sf(
      df[c("hf_name", "hf_as", "hf_zs", "total", "j7", "j14", "j21")],
      geometry = st_geometry(hf_ref)[df$ref_row]
    )
  })()

hf_map <- tm_basemap_epi() +
  tm_shape(adm3, bbox = st_bbox(adm3)) +
  tm_borders(col = "grey60", lwd = 1) +
  tm_shape(adm2) +
  tm_borders(col = "grey20", lwd = 1.3) +
  # carrés de taille fixe, colorés selon le total de cas (plus rouge = plus)
  tm_shape(hf_points) +
  tm_symbols(
    shape = 22,
    size = 1.5,
    fill = "total",
    fill.scale = tm_scale_continuous(values = c("#d7301f", "#7f0000")),
    fill.legend = tm_legend_hide(),
    fill_alpha = 0.92,
    col = "white",
    lwd = 0.8
  ) +
  tm_text("total", size = 0.7, col = "white", fontface = "bold") +
  # étiquettes décalées sous les carrés, placées sans chevauchement
  tm_labels(
    "hf_name",
    size = 0.5,
    col = "grey15",
    fontface = "bold",
    bgcol = "white",
    bgcol_alpha = 0.85,
    ymod = -1.6,
    options = opt_tm_labels(
      remove_overlap = TRUE,
      point.label = TRUE,
      bg.padding = 0.7,
      bg.border = FALSE
    )
  ) +
  tm_theme_epi(
    credits = paste0(
      "Structures ayant vu le plus de cas — couleur selon le total. ",
      n_unmapped,
      " structure(s) non localisée(s) (informelles ou absentes du référentiel FOSA)."
    ),
    date = date_report,
    scalebar_breaks = c(0, 5, 10)
  )
tmap_save(
  hf_map,
  fs::path(plots_dir, "butembo_hf_map.png"),
  height = 8,
  width = 8,
  dpi = 300
)
