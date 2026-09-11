# Script of the TIME analyses in Butembo

source(here::here("R", "0_global.R"))
pos_data_clean <- readRDS(latest_narr_ll_clean)
date_report <- clean_file_date(latest_narr_ll_clean) # export date stamp

#* TIME (Onset) --------------------------------------------------------

# cases with no onset date, excluded from the epicurves
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
    week_symptom_onset = aweek::date2week(
      date_symptom_onset,
      week_start = 1,
      floor_day = TRUE
    )
  )

#* Global weekly epicurve, by infection origin
inf_cols <- c(
  "Local" = "#bc5c5c", # red
  "Imported" = "#3a7ca5", # blue
  "Unknown" = "grey75"
)

# atténue les semaines récentes, vraisemblablement incomplètes (délai de
# notification) : 3 dernières semaines pour le début des symptômes, 1 pour la
# notification
alpha_complete <- 0.7
alpha_recent <- 0.25
n_recent_onset <- 3
n_recent_notif <- 1

# TRUE pour les `n` dernières semaines calendaires présentes dans les données
flag_recent <- function(week_start, n) {
  week_start >= max(week_start) - lubridate::weeks(n - 1)
}

inci_week <- pos_data_clean |>
  filter(!is.na(date_symptom_onset)) |>
  mutate(
    week_symptom_onset = aweek::date2week(
      date_symptom_onset,
      week_start = 1,
      floor_day = TRUE
    ),
    week_start = aweek::week2date(week_symptom_onset)
  ) |>
  count(week_start, infection_butembo) |>
  arrange(week_start) |>
  mutate(
    bar_alpha = if_else(
      flag_recent(week_start, n_recent_onset),
      alpha_recent,
      alpha_complete
    )
  )

nk_conf_epicurve_week <- inci_week |>
  ggplot(aes(x = week_start, y = n, fill = infection_butembo)) +
  geom_col(aes(alpha = bar_alpha), width = 6, colour = "white") +
  scale_alpha_identity() +
  scale_x_date(
    date_breaks = "1 week",
    labels = label_epiweek,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual("Infection", values = inf_cols, drop = FALSE) +
  labs(
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
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

nk_conf_epicurve_week

ggsave(
  fs::path(plots_dir, "butembo_global_epicurve_week.png"),
  nk_conf_epicurve_week,
  height = 7,
  width = 10,
  dpi = 300,
  bg = "white"
)

#* Health Zones, weekly, coloured by health zone
# une entrée par zone de CONFIG$filter_hz, sinon les nouvelles zones sortent en gris
adm2_cols <- c(
  "Butembo" = "#f08080", # salmon
  "Katwa" = "#3a7ca5", # blue
  "Musienene" = "#408323", # green
  "Kalunguta" = "#9a6fb0", # purple
  "Kyondo" = "#e0a458", # ochre
  "Masereka" = "#4f9d9d" # teal
)

inci_adm2_week <- pos_data_clean |>
  filter(!is.na(date_symptom_onset)) |>
  mutate(
    week_symptom_onset = aweek::date2week(
      date_symptom_onset,
      week_start = 1,
      floor_day = TRUE
    ),
    week_start = aweek::week2date(week_symptom_onset)
  ) |>
  count(week_start, adm2_comptabilisation) |>
  arrange(week_start) |>
  mutate(
    bar_alpha = if_else(
      flag_recent(week_start, n_recent_onset),
      alpha_recent,
      alpha_complete
    )
  )

nk_conf_epicurve <- inci_adm2_week |>
  ggplot(aes(x = week_start, y = n, fill = adm2_comptabilisation)) +
  geom_col(aes(alpha = bar_alpha), width = 6, colour = "white") +
  scale_alpha_identity() +
  scale_x_date(
    date_breaks = "1 week",
    labels = label_epiweek,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual("Zone de santé", values = adm2_cols, drop = FALSE) +
  labs(
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
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

nk_conf_epicurve

ggsave(
  fs::path(plots_dir, "butembo_HZ_epicurve_week.png"),
  nk_conf_epicurve,
  height = 7,
  width = 10,
  dpi = 300,
  bg = "white"
)

#* TIME (Notification) -------------------------------------------------

# cases with no notification date, excluded from the epicurves
n_missing_notif <- sum(is.na(pos_data_clean$date_notification))
pct_missing_notif <- round(100 * n_missing_notif / nrow(pos_data_clean))
notif_caption <- glue::glue(
  "Données au {fr_date(date_report)}",
  "\n{n_missing_notif} ({pct_missing_notif}%) cas sans date de notification"
)

#* overall day/ week
inci_notif <- pos_data_clean |>
  filter(!is.na(date_notification)) |>
  count(date_notification) |>
  mutate(
    week_notification = aweek::date2week(
      date_notification,
      week_start = 1,
      floor_day = TRUE
    )
  )

#* Health Zones, weekly, coloured by health zone
inci_adm2_week_notif <- pos_data_clean |>
  filter(!is.na(date_notification)) |>
  mutate(
    week_notification = aweek::date2week(
      date_notification,
      week_start = 1,
      floor_day = TRUE
    ),
    week_start = aweek::week2date(week_notification)
  ) |>
  count(week_start, adm2_comptabilisation) |>
  arrange(week_start) |>
  mutate(
    bar_alpha = if_else(
      flag_recent(week_start, n_recent_notif),
      alpha_recent,
      alpha_complete
    )
  )

nk_conf_epicurve_notif <- inci_adm2_week_notif |>
  ggplot(aes(x = week_start, y = n, fill = adm2_comptabilisation)) +
  geom_col(aes(alpha = bar_alpha), width = 6, colour = "white") +
  scale_alpha_identity() +
  scale_x_date(
    date_breaks = "1 week",
    labels = label_epiweek,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual("Zone de santé", values = adm2_cols, drop = FALSE) +
  labs(
    x = "Semaine de notification",
    y = "Nombre de cas",
    caption = notif_caption
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
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

nk_conf_epicurve_notif

ggsave(
  fs::path(plots_dir, "butembo_HZ_epicurve_week_notif.png"),
  nk_conf_epicurve_notif,
  height = 7,
  width = 10,
  dpi = 300,
  bg = "white"
)
