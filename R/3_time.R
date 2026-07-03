# Script of the TIME analyses in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)

#* TIME (Onset) --------------------------------------------------------

# Cases dropped from the epicurves because they have no onset date.
n_missing_onset <- sum(is.na(pos_data_clean$date_symptom_onset))
pct_missing_onset <- round(100 * n_missing_onset / nrow(pos_data_clean))
onset_caption <- glue::glue(
  "Données au {fr_date(date_report)}",
  "\n{n_missing_onset} ({pct_missing_onset}%) cas sans date de début des symptômes"
)

#* overall day/ week
inci <- pos_data_clean |>
  filter(!is.na(date_symptom_onset)) |>
  count(date_symptom_onset) |>
  mutate(
    week_symptom_onset = aweek::date2week(date_symptom_onset, floor_day = 1)
  )


#* Global weekly epicurve, by infection origin
inf_cols <- c(
  "Locale" = "#bc5c5c", # red
  "Importée" = "#3a7ca5", # blue
  "Incertaine" = "grey75"
)

inci_week <- pos_data_clean |>
  filter(!is.na(date_symptom_onset)) |>
  mutate(
    week_symptom_onset = aweek::date2week(date_symptom_onset, floor_day = 1)
  ) |>
  count(week_symptom_onset, infection_butembo) |>
  mutate(
    week_start = aweek::week2date(week_symptom_onset)
  ) |>
  arrange(week_start)

nk_conf_epicurve_week <- inci_week |>
  ggplot(aes(x = week_start, y = n, fill = infection_butembo)) +
  geom_col(width = 6, alpha = .7, colour = "white") +
  scale_x_date(
    date_breaks = "1 week",
    labels = scales::label_date_short(format = c("%Y", "%b", "%d")),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual("Infection", values = inf_cols, drop = FALSE) +
  labs(
    title = "Cas confirmés de MVE par semaine de début des symptômes",
    subtitle = "Zones de santé de Butembo et Katwa, Nord-Kivu, RDC, 2026",
    x = "Semaine de début des symptômes",
    y = "Nombre de cas",
    caption = onset_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey60",
      linewidth = 0.2,
      linetype = "62"
    ),
    legend.position = "top",
    legend.justification = "left",
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

nk_conf_epicurve_week

ggsave(
  fs::path(out_dir, "butembo_global_epicurve_week.png"),
  nk_conf_epicurve_week,
  height = 6,
  width = 10,
  dpi = 300,
  bg = "white"
)

#* Health Zones, weekly, coloured by health zone
adm2_cols <- c(
  "Butembo" = "#f08080", # salmon
  "Katwa" = "#3a7ca5" # blue
)

inci_adm2_week <- pos_data_clean |>
  filter(!is.na(date_symptom_onset)) |>
  mutate(
    week_symptom_onset = aweek::date2week(date_symptom_onset, floor_day = 1)
  ) |>
  count(week_symptom_onset, adm2_comptabilisation) |>
  mutate(
    week_start = aweek::week2date(week_symptom_onset)
  ) |>
  arrange(week_start)

nk_conf_epicurve <- inci_adm2_week |>
  ggplot(aes(x = week_start, y = n, fill = adm2_comptabilisation)) +
  geom_col(width = 6, alpha = .7, colour = "white") +
  scale_x_date(
    date_breaks = "1 week",
    labels = scales::label_date_short(format = c("%Y", "%b", "%d")),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual("Zone de santé", values = adm2_cols, drop = FALSE) +
  labs(
    title = "Cas confirmés de MVE par semaine de début des symptômes",
    subtitle = "Zones de santé de Butembo et Katwa, Nord-Kivu, RDC, 2026",
    x = "Semaine de début des symptômes",
    y = "Nombre de cas",
    caption = onset_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey60",
      linewidth = 0.2,
      linetype = "62"
    ),
    legend.position = "top",
    legend.justification = "left",
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

nk_conf_epicurve

ggsave(
  fs::path(out_dir, "butembo_HZ_epicurve_week.png"),
  nk_conf_epicurve,
  height = 6,
  width = 10,
  dpi = 300,
  bg = "white"
)
