# ! Script of the DELAYS situation in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)

#* DELAYS -------------------------------------------------------------
# Three delays, each on its own denominator:
#   - onset -> notification : all cases with both dates
#   - onset -> death        : cases with type_of_exit == "Décédé"
#   - onset -> cure         : cases with type_of_exit == "Guéri"
# The event dates come from the cleaned linelist (already parsed as Date).
delays <- pos_data_clean |>
  select(
    date_symptom_onset,
    date_notification,
    date_exit_eff,
    type_of_exit
  ) |>
  mutate(
    delay_ons_not = as.numeric(date_notification - date_symptom_onset),
    delay_ons_death = if_else(
      type_of_exit == "Décédé",
      as.numeric(date_exit_eff - date_symptom_onset),
      NA_real_
    ),
    delay_ons_cure = if_else(
      type_of_exit == "Guéri",
      as.numeric(date_exit_eff - date_symptom_onset),
      NA_real_
    )
  )

# Readable labels + a fixed display order for the three delays
delay_labels <- c(
  "delay_ons_not" = "Début des symptômes → notification",
  "delay_ons_death" = "Début des symptômes → décès",
  "delay_ons_cure" = "Début des symptômes → guérison"
)
delay_cols <- c(
  "Début des symptômes → notification" = "#3a7ca5", # blue
  "Début des symptômes → décès" = "#bc5c5c", # red
  "Début des symptômes → guérison" = "#66a61e" # green
)

# Onset -> notification is shown separately (stratified by outcome, below), so
# the distribution / boxplot / table cover only the two onset -> exit delays.
exit_labels <- delay_labels[c("delay_ons_death", "delay_ons_cure")]

delays_long <- delays |>
  select(delay_ons_death, delay_ons_cure) |>
  pivot_longer(
    cols = everything(),
    names_to = "delay_name",
    values_to = "delay"
  ) |>
  filter(!is.na(delay)) |>
  mutate(
    delay_name = factor(
      delay_labels[delay_name],
      levels = unname(exit_labels)
    )
  )

#* Distribution of delays ---------------------------------------------
butembo_delays_hist <- delays_long |>
  ggplot(aes(x = delay, fill = delay_name)) +
  # boundary = -0.5 aligns each 1-day bin on its integer value (centred on ticks)
  geom_histogram(binwidth = 1, boundary = -0.5, colour = "white", alpha = .8) +
  facet_wrap(~delay_name, ncol = 1, scales = "free_y") +
  scale_x_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(values = delay_cols, guide = "none") +
  labs(
    title = "Distribution des délais — cas confirmés de MVE",
    subtitle = "Zones de santé de Butembo et Katwa, Nord-Kivu, RDC, 2026",
    x = "Délai (jours)",
    y = "Nombre de cas",
    caption = paste0("Données au ", fr_date(date_report))
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
    strip.text = element_text(face = "bold", hjust = 0, size = 11),
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_delays_hist

ggsave(
  fs::path(out_dir, "butembo_delays_distribution.png"),
  butembo_delays_hist,
  height = 8,
  width = 9,
  dpi = 300,
  bg = "white"
)

#* Boxplot + jitter of delays -----------------------------------------
butembo_delays_box <- delays_long |>
  ggplot(aes(
    x = delay,
    y = delay_name,
    colour = delay_name,
    fill = delay_name
  )) +
  geom_boxplot(
    width = 0.5,
    alpha = 0.25,
    outlier.shape = NA, # points shown by the jitter layer instead
    linewidth = 0.4
  ) +
  geom_jitter(
    height = 0.15,
    width = 0, # keep the exact delay value on x
    size = 1.8,
    alpha = 0.6
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_discrete(limits = rev) + # first delay on top
  scale_colour_manual(values = delay_cols, guide = "none") +
  scale_fill_manual(values = delay_cols, guide = "none") +
  labs(
    title = "Délais — cas confirmés de MVE",
    subtitle = "Zones de santé de Butembo et Katwa, Nord-Kivu, RDC, 2026",
    x = "Délai (jours)",
    y = NULL,
    caption = paste0("Données au ", fr_date(date_report))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey80",
      linewidth = 0.2,
      linetype = "62"
    ),
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_delays_box

ggsave(
  fs::path(out_dir, "butembo_delays_boxplot.png"),
  butembo_delays_box,
  height = 5,
  width = 12,
  dpi = 300,
  bg = "white"
)

#* Onset -> notification delay, stratified by outcome -----------------
# Does the time from symptom onset to notification differ between cases that
# died and those that recovered? Restricted to resolved cases.
outcome_cols <- c(
  "Décédé" = "#bc5c5c", # red
  "Guéri" = "#66a61e" # green
)

notif_by_outcome <- delays |>
  filter(type_of_exit %in% c("Décédé", "Guéri"), !is.na(delay_ons_not)) |>
  mutate(outcome = factor(type_of_exit, levels = c("Décédé", "Guéri")))

butembo_delay_notif_outcome <- notif_by_outcome |>
  ggplot(aes(
    x = delay_ons_not,
    y = outcome,
    colour = outcome,
    fill = outcome
  )) +
  geom_boxplot(
    width = 0.5,
    alpha = 0.25,
    outlier.shape = NA,
    linewidth = 0.4
  ) +
  geom_jitter(
    height = 0.15,
    width = 0,
    size = 1.8,
    alpha = 0.6
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_discrete(limits = rev) +
  scale_colour_manual(values = outcome_cols, guide = "none") +
  scale_fill_manual(values = outcome_cols, guide = "none") +
  labs(
    title = "Délai début des symptômes → notification, selon l'issue",
    subtitle = "Cas confirmés de MVE — Butembo et Katwa, Nord-Kivu, RDC, 2026",
    x = "Délai début des symptômes → notification (jours)",
    y = NULL,
    caption = paste0("Données au ", fr_date(date_report))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      colour = "grey80",
      linewidth = 0.2,
      linetype = "62"
    ),
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_delay_notif_outcome

ggsave(
  fs::path(out_dir, "butembo_delay_notif_by_outcome.png"),
  butembo_delay_notif_outcome,
  height = 4.5,
  width = 9,
  dpi = 300,
  bg = "white"
)

#* Median delay table -------------------------------------------------
# Helper: n / median / IQR for a delay vector
delay_stats <- function(x) {
  tibble::tibble(
    n = sum(!is.na(x)),
    median = median(x, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE)
  )
}

# Onset -> notification, stratified by outcome (matches the stratified figure)
notif_median <- notif_by_outcome |>
  reframe(.by = outcome, delay_stats(delay_ons_not)) |>
  mutate(
    delay_name = paste0(
      "Début des symptômes → notification (",
      if_else(outcome == "Décédé", "décédés", "guéris"),
      ")"
    )
  ) |>
  select(delay_name, n, median, q25, q75)

# Onset -> exit delays (death / cure)
exit_median <- delays_long |>
  summarise(
    .by = delay_name,
    n = n(),
    median = median(delay, na.rm = TRUE),
    q25 = quantile(delay, 0.25, na.rm = TRUE),
    q75 = quantile(delay, 0.75, na.rm = TRUE)
  ) |>
  mutate(delay_name = as.character(delay_name))

delays_median <- bind_rows(notif_median, exit_median)

delays_median_gt <- delays_median |>
  mutate(iqr = paste0(q25, " – ", q75)) |>
  select(delay_name, n, median, iqr) |>
  gt::gt() |>
  gt::cols_label(
    delay_name = "Délai",
    n = "N",
    median = "Médiane (jours)",
    iqr = "IQR (jours)"
  ) |>
  gt::cols_align(align = "center", columns = c(n, median, iqr)) |>
  gt::tab_caption(
    gt::md(paste0(
      "**Délais médians — données au ",
      fr_date(date_report),
      "**"
    ))
  ) |>
  gt::tab_footnote(
    "IQR = intervalle interquartile (25e–75e percentile)."
  )

delays_median_gt |>
  save_gt("butembo_delays_median.png")
