# ! Script of the PERSON situation in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)

#* PERSON (Age & sex pyramid) ------------------------------------------

pyr_data <- pos_data_clean |>
  filter(!is.na(age), !is.na(sex)) |>
  count(infection_butembo, age_group, sex, .drop = FALSE) |>
  tidyr::complete(
    age_group,
    sex,
    fill = list(n = 0)
  ) |>
  mutate(
    n_signed = if_else(sex == "Homme", -n, n),
    mid = n_signed / 2,
    y = as.integer(age_group)
  )

# symmetric x-axis rounded out to the next even number
x_max <- max(2, ceiling(max(pyr_data$n) / 2) * 2)

sex_cols <- c(
  "Homme" = "#5d8f76", # muted teal
  "Femme" = "#d0b13f" # turquoise
)

nk_pyramid <- pyr_data |>
  ggplot(aes(x = n_signed, y = age_group, fill = sex)) +
  geom_col(width = 0.9, colour = "white", linewidth = 0.3, alpha = .7) +
  geom_vline(xintercept = 0, colour = "grey30", linewidth = 0.4) +
  geom_segment(
    aes(
      x = mid,
      xend = mid,
      y = y - 0.45,
      yend = y + 0.45,
      linetype = "Point médian"
    ),
    colour = "grey25",
    linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(-x_max, x_max, 2),
    labels = function(x) abs(x),
    limits = c(-x_max, x_max),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(values = sex_cols) +
  scale_linetype_manual(NULL, values = c("Point médian" = "22")) +
  labs(
    title = "Répartition des cas confirmés de MVE par âge, sexe et type d'infection",
    subtitle = "Zones de santé de Butembo et Katwa, Nord-Kivu, RDC, 2026",
    x = "Nombre de cas",
    y = "Groupe d'âge (années)",
    fill = NULL,
    caption = paste0("Données au ", fr_date(date_report))
  ) +
  guides(
    linetype = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  ) +
  facet_wrap(~infection_butembo) +
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
    plot.margin = margin(10, 14, 10, 10)
  )

nk_pyramid

ggsave(
  fs::path(out_dir, "butembo_age_pyramid.png"),
  nk_pyramid,
  height = 5,
  width = 8,
  dpi = 300,
  bg = "white"
)

#* MORTALITÉ (décès à l'arrivée & décès communautaire) -----------------
# Petite synthèse : nombre (et %) de cas décédés à l'arrivée à l'ETC et
# décès survenus dans la communauté. Le % est calculé parmi les cas dont
# l'information est renseignée (hors valeurs manquantes).
mort_desc <- tibble::tibble(
  indicateur = c(
    "Décès à l'arrivée à l'ETC",
    "Décès communautaire"
  ),
  n_oui = c(
    sum(pos_data_clean$dead_upon_arrival == "Oui", na.rm = TRUE),
    sum(pos_data_clean$community_death == "Oui", na.rm = TRUE)
  ),
  n_connu = c(
    sum(!is.na(pos_data_clean$dead_upon_arrival)),
    sum(!is.na(pos_data_clean$community_death))
  )
) |>
  mutate(pct = n_oui / n_connu)

mort_desc_gt <- mort_desc |>
  gt::gt() |>
  gt::cols_label(
    indicateur = "",
    n_oui = "Cas",
    n_connu = "Renseignés",
    pct = "%"
  ) |>
  gt::fmt_percent(pct, decimals = 1) |>
  gt::tab_caption(
    gt::md(paste0(
      "**Décès à l'arrivée et décès communautaires — cas confirmés — données au ",
      fr_date(date_report),
      "**"
    ))
  ) |>
  gt::tab_footnote(
    "% calculé parmi les cas avec information renseignée (hors valeurs manquantes)."
  )

mort_desc_gt |>
  save_gt("butembo_mortality_desc.png")
