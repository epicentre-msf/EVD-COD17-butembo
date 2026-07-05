# ! Script of the DELAYS situation in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)

#* DELAYS -------------------------------------------------------------
# onset -> notification (all cases), onset -> death (décès), onset -> cure (guéris)
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

# case counts for the completeness captions
n_all_total <- nrow(delays)
n_death_total <- sum(delays$type_of_exit == "Décédé", na.rm = TRUE)
n_cure_total <- sum(delays$type_of_exit == "Guéri", na.rm = TRUE)

n_notif_valid <- sum(!is.na(delays$delay_ons_not))
n_death_valid <- sum(!is.na(delays$delay_ons_death))
n_cure_valid <- sum(!is.na(delays$delay_ons_cure))
n_notif_death_valid <- sum(
  delays$type_of_exit == "Décédé" & !is.na(delays$delay_ons_not)
)
n_notif_cure_valid <- sum(
  delays$type_of_exit == "Guéri" & !is.na(delays$delay_ons_not)
)

# labels and display order
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

# notification delay handled separately below; keep only the two exit delays
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

#* MLE fit of the onset -> death delay --------------------------------
# gamma fit (fitdistrplus, MLE)
death_delays <- delays |>
  filter(type_of_exit == "Décédé", !is.na(delay_ons_death)) |>
  pull(delay_ons_death)

# fitdistrplus needs positive values; nudge same-day deaths to 0.5
death_delays_fit <- if_else(death_delays <= 0, 0.5, death_delays)

fit_gamma <- fitdistrplus::fitdist(death_delays_fit, "gamma", method = "mle")

# fitted parameters and quantiles
gamma_shape <- unname(fit_gamma$estimate["shape"])
gamma_rate <- unname(fit_gamma$estimate["rate"])
gamma_mean <- gamma_shape / gamma_rate
gamma_q <- qgamma(c(0.25, 0.5, 0.75), shape = gamma_shape, rate = gamma_rate)

# density curve scaled to counts for the histogram overlay
x_grid <- seq(0, max(death_delays_fit), length.out = 300)
gamma_curve <- tibble::tibble(
  x = x_grid,
  count = dgamma(x_grid, shape = gamma_shape, rate = gamma_rate) *
    length(death_delays_fit)
)

fit_subtitle <- sprintf(
  "Ajustement gamma (MLE) — forme = %.2f, taux = %.2f · moyenne = %.1f j, médiane = %.1f j (n = %d)",
  gamma_shape,
  gamma_rate,
  gamma_mean,
  gamma_q[2],
  length(death_delays_fit)
)

butembo_delay_death_fit <- tibble::tibble(delay = death_delays_fit) |>
  ggplot(aes(x = delay)) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5,
    colour = "white",
    fill = delay_cols[["Début des symptômes → décès"]],
    alpha = 0.8
  ) +
  geom_line(
    data = gamma_curve,
    aes(x = x, y = count),
    colour = "#7a1f1f",
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = gamma_q[2],
    colour = "#7a1f1f",
    linetype = "22",
    linewidth = 0.5
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(5),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Délai (jours)",
    y = "Nombre de cas",
    caption = paste0(
      fit_subtitle,
      "\nDécès : ",
      n_death_valid,
      " / ",
      n_death_total,
      " cas avec information disponible\n",
      "Données au ",
      fr_date(date_report)
    )
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
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 9),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_delay_death_fit

ggsave(
  fs::path(out_dir, "butembo_delay_death_gamma_fit.png"),
  butembo_delay_death_fit,
  height = 6,
  width = 8,
  dpi = 300,
  bg = "white"
)

#* Delay-fit database -------------------------------------------------
# one row per fitted delay; fit list-column keeps the full fitdistrplus object
delay_fits <- tibble::tibble(
  delay = "onset_to_death",
  delay_label = "Début des symptômes → décès",
  distribution = "gamma",
  method = "mle",
  n = length(death_delays_fit),
  shape = gamma_shape,
  rate = gamma_rate,
  mean = gamma_mean,
  median = gamma_q[2],
  q25 = gamma_q[1],
  q75 = gamma_q[3],
  aic = fit_gamma$aic,
  loglik = fit_gamma$loglik,
  fit = list(fit_gamma),
  date_updated = date_report
)

file_base <- glue::glue("BUT-EVD_BUTEMBO_delay-fits__{time_stamp()}")

saveRDS(
  delay_fits,
  fs::path(
    butembo_project_clean_data_path,
    paste0(file_base, ".rds")
  )
)

#* Boxplot + jitter of delays -----------------------------------------
butembo_delays_box <- delays_long |>
  ggplot(aes(
    x = delay_name,
    y = delay,
    colour = delay_name,
    fill = delay_name
  )) +
  geom_boxplot(
    width = 0.3,
    alpha = 0.1,
    outlier.shape = NA,
    linewidth = 0.4
  ) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 1.8,
    alpha = 0.4
  ) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(
    breaks = scales::breaks_width(5),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_colour_manual(values = delay_cols, guide = "none") +
  scale_fill_manual(values = delay_cols, guide = "none") +
  labs(
    x = NULL,
    y = "Délai (jours)",
    caption = paste0(
      "Décès : ",
      n_death_valid,
      " / ",
      n_death_total,
      " · Guérison : ",
      n_cure_valid,
      " / ",
      n_cure_total,
      " cas avec information disponible\n",
      "Données au ",
      fr_date(date_report)
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey80",
      linewidth = 0.2,
      linetype = "62"
    ),
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.ticks.y = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_delays_box

#* Onset -> notification delay, stratified by outcome -----------------
# restricted to resolved cases (décédés / guéris)
outcome_cols <- c(
  "Décédé" = "#6a4c93", # purple
  "Guéri" = "#e69f00" # orange
)

notif_by_outcome <- delays |>
  filter(type_of_exit %in% c("Décédé", "Guéri"), !is.na(delay_ons_not)) |>
  mutate(outcome = factor(type_of_exit, levels = c("Décédé", "Guéri")))

butembo_delay_notif_outcome <- notif_by_outcome |>
  ggplot(aes(
    x = outcome,
    y = delay_ons_not,
    colour = outcome,
    fill = outcome
  )) +
  geom_boxplot(
    width = 0.3,
    alpha = 0.12,
    outlier.shape = NA,
    linewidth = 0.4
  ) +
  geom_jitter(
    width = 0.15,
    height = 0,
    size = 1.8,
    alpha = 0.6
  ) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_colour_manual(values = outcome_cols, guide = "none") +
  scale_fill_manual(values = outcome_cols, guide = "none") +
  labs(
    x = NULL,
    y = "Délai début des symptômes → notification (jours)",
    caption = paste0(
      "Décédés : ",
      n_notif_death_valid,
      " / ",
      n_death_total,
      " · Guéris : ",
      n_notif_cure_valid,
      " / ",
      n_cure_total,
      " cas avec information disponible\n",
      "Données au ",
      fr_date(date_report)
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey80",
      linewidth = 0.2,
      linetype = "62"
    ),
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.ticks.y = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_delay_notif_outcome

#* Combined boxplots (patchwork) --------------------------------------
# exit delays + notification-by-outcome side by side
butembo_delays_combined <- (butembo_delays_box / butembo_delay_notif_outcome) +
  patchwork::plot_annotation(
    tag_levels = "A",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

butembo_delays_combined

ggsave(
  fs::path(out_dir, "butembo_delays_combined.png"),
  butembo_delays_combined,
  height = 11,
  width = 8,
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
  gt::tab_source_note(
    gt::md(paste0("Données au ", fr_date(date_report)))
  ) |>
  gt::tab_footnote(
    "IQR = intervalle interquartile (25e–75e percentile)."
  )

delays_median_gt |>
  save_gt("butembo_delays_median.png")
