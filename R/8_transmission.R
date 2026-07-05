# ! Script of the TRANSMISSION situation in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)

n_total <- nrow(pos_data_clean)

# lien connu = mécanisme de transmission renseigné (hors "Inconnu")
trans_dat <- pos_data_clean |>
  select(
    infection_butembo,
    transmission_type_1,
    HF_name_transmission_1,
    contact_tracing_known_yn,
    age_group,
    sex
  ) |>
  mutate(
    lien_connu = if_else(
      is.na(transmission_type_1) | transmission_type_1 == "Inconnu",
      "Lien inconnu",
      "Lien connu"
    ),
    lien_connu = factor(lien_connu, levels = c("Lien connu", "Lien inconnu")),
    # Was the case registered/followed as a known contact?
    contact_connu = case_match(
      contact_tracing_known_yn,
      "Oui" ~ "Oui",
      "Non" ~ "Non",
      .default = "Incertain / inconnu"
    ),
    contact_connu = factor(
      contact_connu,
      levels = c("Oui", "Non", "Incertain / inconnu")
    ),
    # Familial and care-of-the-sick transmission are always reported together
    transmission_type_1 = forcats::fct_collapse(
      transmission_type_1,
      "Familiale / soins au malade" = c("Familiale", "Soins au malade")
    )
  )

#* 1. Overview table — transmission link & infection origin -----------
# Transmission types among LOCAL infections (% of local cases),
# with "Incertaine" and "Inconnu" bundled together
local_type_tbl <- trans_dat |>
  filter(infection_butembo == "Locale") |>
  mutate(
    niveau = as.character(transmission_type_1),
    niveau = if_else(is.na(niveau), "Inconnu", niveau),
    niveau = if_else(
      niveau %in% c("Incertaine", "Inconnu"),
      "Incertaine / inconnu",
      niveau
    )
  ) |>
  count(niveau) |>
  mutate(
    categorie = "Type de transmission (infections locales)",
    pct = n / sum(n)
  ) |>
  arrange(desc(n))

overview_tbl <- bind_rows(
  trans_dat |>
    count(niveau = lien_connu, .drop = FALSE) |>
    mutate(categorie = "Lien de transmission", pct = n / n_total),
  trans_dat |>
    count(niveau = infection_butembo, .drop = FALSE) |>
    mutate(categorie = "Origine de l'infection", pct = n / n_total),
  trans_dat |>
    count(niveau = contact_connu, .drop = FALSE) |>
    mutate(categorie = "Suivi comme contact", pct = n / n_total),
  local_type_tbl
) |>
  mutate(niveau = as.character(niveau)) |>
  select(categorie, niveau, n, pct)

overview_gt <- overview_tbl |>
  gt::gt(
    rowname_col = "niveau",
    groupname_col = "categorie"
  ) |>
  gt::cols_label(
    n = "Cas",
    pct = "%"
  ) |>
  gt::fmt_percent(pct, decimals = 0) |>
  gt::cols_align(align = "center", columns = c(n, pct)) |>
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_row_groups()
  ) |>
  gt::tab_style(
    style = gt::cell_text(align = "left"),
    locations = gt::cells_stub()
  ) |>
  gt::tab_source_note(
    gt::md(paste0("Données au ", fr_date(date_report)))
  ) |>
  gt::tab_footnote(
    "Type de transmission : % parmi les cas acquis localement.",
    locations = gt::cells_row_groups(
      groups = "Type de transmission (infections locales)"
    )
  )

overview_gt |>
  save_gt("butembo_transmission_overview.png")

#* 3. Age & sex — nosocomial vs all other transmission ----------------
sex_cols <- c(
  "Homme" = "#5d8f76", # muted teal
  "Femme" = "#d0b13f" # gold
)

pyr_data <- trans_dat |>
  filter(!is.na(age_group), !is.na(sex)) |>
  mutate(
    groupe = if_else(
      !is.na(transmission_type_1) & transmission_type_1 == "Nosocomiale",
      "Nosocomiale",
      "Autres transmissions"
    ),
    groupe = factor(groupe, levels = c("Nosocomiale", "Autres transmissions"))
  ) |>
  count(groupe, age_group, sex, .drop = FALSE) |>
  tidyr::complete(groupe, age_group, sex, fill = list(n = 0)) |>
  mutate(n_signed = if_else(sex == "Homme", -n, n))

x_max <- max(2, ceiling(max(pyr_data$n) / 2) * 2)

butembo_noso_pyramid <- pyr_data |>
  ggplot(aes(x = n_signed, y = age_group, fill = sex)) +
  geom_col(width = 0.9, colour = "white", linewidth = 0.3, alpha = .7) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
  scale_x_continuous(
    breaks = seq(-x_max, x_max, 2),
    labels = function(x) abs(x),
    limits = c(-x_max, x_max),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(values = sex_cols) +
  labs(
    x = "Nombre de cas",
    y = "Groupe d'âge (années)",
    fill = NULL,
    caption = paste0("Données au ", fr_date(date_report))
  ) +
  facet_wrap(~groupe, nrow = 1) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      colour = "grey80",
      linewidth = 0.2,
      linetype = "62"
    ),
    legend.position = "top",
    legend.justification = "left",
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_noso_pyramid

ggsave(
  fs::path(out_dir, "butembo_nosocomial_age_sex.png"),
  butembo_noso_pyramid,
  height = 6,
  width = 8,
  dpi = 300,
  bg = "white"
)
