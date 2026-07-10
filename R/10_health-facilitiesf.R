# ! Script of the patient HF travels (parcours de soins avant isolement)

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)

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
  fs::path(out_dir, "butembo_hf_visits_distribution.png"),
  butembo_hf_dist,
  height = 8,
  width = 7,
  dpi = 300,
  bg = "white"
)

#* 10 most-visited structures — last 3 weeks --------------------------
window_start <- date_report - 21

top_structures <- hf_visits |>
  filter(
    !is.na(date_start_HF_visited),
    date_start_HF_visited >= window_start,
    date_start_HF_visited <= date_report
  ) |>
  count(hf_name, hf_as, hf_zs, name = "n_visites", sort = TRUE) |>
  slice_head(n = 10)

top_structures_gt <- top_structures |>
  gt::gt() |>
  gt::cols_label(
    hf_name = "Structure",
    hf_as = "Aire de santé",
    hf_zs = "Zone de santé",
    n_visites = "Visites"
  ) |>
  gt::cols_align(align = "center", columns = n_visites) |>
  gt::tab_source_note(
    paste0(
      "Visites enregistrées du ",
      fr_date(window_start),
      " au ",
      fr_date(date_report),
      "."
    )
  )

top_structures_gt |>
  save_gt("butembo_hf_top_structures.png")

#* 10 structures ayant notifié le plus de cas — last 3 weeks ----------
# reporting structure = where the case was located on its notification date
notif_lookup <- pos_data_clean |>
  transmute(patient_name, date_notification = as.Date(date_notification)) |>
  filter(!is.na(date_notification)) |>
  distinct()

reporting_structures <- hf_visits |>
  inner_join(notif_lookup, by = "patient_name") |>
  filter(
    date_notification >= date_start_HF_visited,
    is.na(date_end_HF_visited) | date_notification <= date_end_HF_visited,
    date_notification >= window_start,
    date_notification <= date_report
  ) |>
  # if several overlapping stays match, keep the most recent one
  slice_max(date_start_HF_visited, by = patient_name, n = 1, with_ties = FALSE) |>
  count(hf_name, hf_as, hf_zs, name = "n_cas", sort = TRUE) |>
  slice_head(n = 10)

reporting_structures_gt <- reporting_structures |>
  gt::gt() |>
  gt::cols_label(
    hf_name = "Structure",
    hf_as = "Aire de santé",
    hf_zs = "Zone de santé",
    n_cas = "Cas notifiés"
  ) |>
  gt::cols_align(align = "center", columns = n_cas) |>
  gt::tab_source_note(
    paste0(
      "Structure où se trouvait le cas à la date de notification, du ",
      fr_date(window_start),
      " au ",
      fr_date(date_report),
      "."
    )
  )

reporting_structures_gt |>
  save_gt("butembo_hf_reporting_structures.png")
