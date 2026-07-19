# Analysis of Butembo transmission pairs

source(here::here("R", "0_global.R"))
library(tidyverse)

ll <- readRDS(latest_narr_ll_clean)$data

trans_dat <- readRDS(latest_transmission_clean)

# Serial interval
# look at the delay between infecto DDS and infecteed DDS (from the ll)

# dataset from excel
df <- trans_dat |>
  select(from, to, start_exposure, end_exposure) |>
  left_join(
    select(ll, from_onset = date_symptom_onset, pid),
    by = join_by(from == pid)
  ) |>
  left_join(
    select(ll, to_onset = date_symptom_onset, pid),
    by = join_by(to == pid)
  ) |>
  mutate(
    si = as.numeric(to_onset - from_onset),
    exp_window = as.numeric(end_exposure - start_exposure)
  )

# Exploratory

# max SI is 29 !!
df |> filter(si == max(si, na.rm = TRUE))

# ! dataset from ll - NEED TO UPDATE
ll |>
  filter(
    !is.na(infector_ID_1) &
      !infector_ID_1 %in%
        c(
          "plusieurs hypothèses"
        )
  ) |>
  select(to = pid, to_onset = date_symptom_onset, from = infector_ID_1) |>
  left_join(
    select(ll, from_onset = date_symptom_onset, pid),
    by = join_by(from == pid)
  )

#! SERIAL INTERVAL ------------------------------

# Serial interval = onset(infectee) - onset(infector), in days.
# Gamma / lognormal / Weibull are all defined on strictly positive support, so
# the fit uses only pairs with a positive SI (negatives = co-primary / ordering
# noise); the histogram below shows only those same values so it lines up with
# the fitted curves.
si_vals <- df |>
  filter(!is.na(si) & si > 0) |>
  pull(si)

n_dropped <- sum(!is.na(df$si) & df$si <= 0)

fit_si_gamma <- fitdistrplus::fitdist(si_vals, "gamma", method = "mle")
fit_si_lnorm <- fitdistrplus::fitdist(si_vals, "lnorm", method = "mle")
fit_si_weib <- fitdistrplus::fitdist(
  si_vals,
  "weibull",
  method = "mle",
  start = list(shape = 1, scale = mean(si_vals))
)

# fitted curves over a smooth grid for ggplot overlay. The histogram is on the
# count scale, so each density is scaled by n * binwidth to convert it into an
# expected number of pairs per bin.
si_cols <- c(
  "Gamma" = "#3a7ca5", # blue
  "Log-normale" = "#bc5c5c", # red
  "Weibull" = "#66a61e" # green
)

si_binwidth <- 1
si_scale <- length(si_vals) * si_binwidth

si_grid <- seq(0, max(si_vals), length.out = 200)
si_curves <- bind_rows(
  tibble(
    x = si_grid,
    count = si_scale *
      dgamma(
        si_grid,
        shape = fit_si_gamma$estimate[["shape"]],
        rate = fit_si_gamma$estimate[["rate"]]
      ),
    distribution = "Gamma"
  ),
  tibble(
    x = si_grid,
    count = si_scale *
      dlnorm(
        si_grid,
        meanlog = fit_si_lnorm$estimate[["meanlog"]],
        sdlog = fit_si_lnorm$estimate[["sdlog"]]
      ),
    distribution = "Log-normale"
  ),
  tibble(
    x = si_grid,
    count = si_scale *
      dweibull(
        si_grid,
        shape = fit_si_weib$estimate[["shape"]],
        scale = fit_si_weib$estimate[["scale"]]
      ),
    distribution = "Weibull"
  )
) |>
  mutate(distribution = factor(distribution, levels = names(si_cols)))

butembo_si_hist <- ggplot() +
  geom_histogram(
    data = tibble(si = si_vals),
    aes(x = si),
    binwidth = si_binwidth,
    colour = "white",
    fill = "grey75",
    alpha = .8
  ) +
  geom_line(
    data = si_curves,
    aes(x = x, y = count, colour = distribution),
    linewidth = 0.9
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_colour_manual("Distribution ajustée", values = si_cols) +
  labs(
    title = "Intervalle sériel — cas confirmés de MVE",
    subtitle = "Zones de santé de Butembo et Katwa, Nord-Kivu, RDC, 2026",
    x = "Intervalle sériel (jours)",
    y = "Nombre de paires",
    caption = paste0(
      "n = ",
      length(si_vals),
      " paires de transmission (intervalle sériel > 0)"
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
    legend.position = "top",
    legend.justification = "left",
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_si_hist

ggsave(
  fs::path(out_dir, "butembo_serial_interval.png"),
  butembo_si_hist,
  height = 6,
  width = 9,
  dpi = 300,
  bg = "white"
)

#! Incubation period ------------------------------

inc_walk <- df |>
  filter(!is.na(start_exposure) & !is.na(end_exposure) & !is.na(to_onset)) |>
  select(to, start_exposure, end_exposure, to_onset) |>
  # one row per (case, candidate infection day)
  rowwise() |>
  reframe(
    to = to,
    to_onset = to_onset,
    infection_day = seq(start_exposure, end_exposure, by = "day")
  ) |>
  mutate(delay = as.numeric(to_onset - infection_day))

# Keep only possible delays (infection before onset) and give each CASE total
# weight 1, split evenly over its candidate days (weight = 1 / n days). This is
# what stops a wide, uncertain exposure window from dominating the shape.
inc_fit <- inc_walk |>
  filter(delay > 0) |>
  group_by(to) |>
  mutate(weight = 1 / dplyr::n()) |>
  ungroup()

n_inc_cases <- dplyr::n_distinct(inc_fit$to)

fit_inc_gamma <- fitdistrplus::fitdist(
  inc_fit$delay,
  "gamma",
  method = "mle"
)
fit_inc_lnorm <- fitdistrplus::fitdist(
  inc_fit$delay,
  "lnorm",
  method = "mle"
)
fit_inc_weib <- fitdistrplus::fitdist(
  inc_fit$delay,
  "weibull",
  method = "mle",
  start = list(shape = 1, scale = mean(inc_fit$delay))
)

# fitted curves scaled to the count axis. The weighted histogram sums to
# n_inc_cases (each case = weight 1), so scale each density by n_cases * binwidth.
inc_cols <- c(
  "Gamma" = "#3a7ca5", # blue
  "Log-normale" = "#bc5c5c", # red
  "Weibull" = "#66a61e" # green
)

inc_binwidth <- 1
inc_scale <- n_inc_cases * inc_binwidth

inc_grid <- seq(0, max(inc_fit$delay), length.out = 200)
inc_curves <- bind_rows(
  tibble(
    x = inc_grid,
    count = inc_scale *
      dgamma(
        inc_grid,
        shape = fit_inc_gamma$estimate[["shape"]],
        rate = fit_inc_gamma$estimate[["rate"]]
      ),
    distribution = "Gamma"
  ),
  tibble(
    x = inc_grid,
    count = inc_scale *
      dlnorm(
        inc_grid,
        meanlog = fit_inc_lnorm$estimate[["meanlog"]],
        sdlog = fit_inc_lnorm$estimate[["sdlog"]]
      ),
    distribution = "Log-normale"
  ),
  tibble(
    x = inc_grid,
    count = inc_scale *
      dweibull(
        inc_grid,
        shape = fit_inc_weib$estimate[["shape"]],
        scale = fit_inc_weib$estimate[["scale"]]
      ),
    distribution = "Weibull"
  )
) |>
  mutate(distribution = factor(distribution, levels = names(inc_cols)))

butembo_inc_hist <- ggplot() +
  geom_histogram(
    data = inc_fit,
    aes(x = delay, weight = weight),
    binwidth = inc_binwidth,
    colour = "white",
    fill = "grey75",
    alpha = .8
  ) +
  geom_line(
    data = inc_curves,
    aes(x = x, y = count, colour = distribution),
    linewidth = 0.9
  ) +
  scale_x_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_colour_manual("Distribution ajustée", values = inc_cols) +
  labs(
    title = "Période d'incubation — cas confirmés de MVE",
    subtitle = "Zones de santé de Butembo et Katwa, Nord-Kivu, RDC, 2026",
    x = "Période d'incubation (jours)",
    y = "Nombre de cas (pondéré)",
    caption = paste0(
      "n = ",
      n_inc_cases,
      " cas ; chaque cas réparti uniformément sur sa fenêtre d'exposition"
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
    legend.position = "top",
    legend.justification = "left",
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_inc_hist

ggsave(
  fs::path(out_dir, "butembo_incubation_period.png"),
  butembo_inc_hist,
  height = 6,
  width = 9,
  dpi = 300,
  bg = "white"
)
