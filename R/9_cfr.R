# ! Script of the CFR (létalité) situation in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)

#* Onset -> death delay distribution ----------------------------------
# gamma fit from R/5_delays.R, used to adjust the CFR for unresolved cases
delay_fit_files <- fs::dir_ls(
  butembo_project_clean_data_path,
  regexp = "BUTEMBO_delay-fits"
)

onset_death_fit <- readRDS(max(delay_fit_files)) |>
  filter(delay == "onset_to_death")

delay_density <- function(x) {
  dgamma(x, shape = onset_death_fit$shape, rate = onset_death_fit$rate)
}

#* Daily incidence for the cfr package --------------------------------

death_dates <- pos_data_clean$date_exit_eff[
  pos_data_clean$type_of_exit == "Décédé"
]
date_min <- min(pos_data_clean$date_symptom_onset, na.rm = TRUE)
date_max <- max(c(pos_data_clean$date_symptom_onset, death_dates), na.rm = TRUE)

build_cfr_data <- function(df, date_min, date_max) {
  onsets <- df |>
    filter(!is.na(date_symptom_onset)) |>
    count(date = date_symptom_onset, name = "cases")
  deaths <- df |>
    filter(type_of_exit == "Décédé", !is.na(date_exit_eff)) |>
    count(date = date_exit_eff, name = "deaths")
  tibble(date = seq(date_min, date_max, by = "day")) |>
    left_join(onsets, by = "date") |>
    left_join(deaths, by = "date") |>
    mutate(
      cases = tidyr::replace_na(cases, 0L),
      deaths = tidyr::replace_na(deaths, 0L)
    )
}

cfr_data <- build_cfr_data(pos_data_clean, date_min, date_max)

#* 1. Overall CFR — crude vs delay-adjusted ---------------------------
cfr_naive <- cfr::cfr_static(cfr_data)
cfr_adj <- cfr::cfr_static(cfr_data, delay_density = delay_density)

cfr_tbl <- bind_rows(
  cfr_naive |> mutate(method = "Brute (non ajustée)"),
  cfr_adj |> mutate(method = "Ajustée (délai début → décès)")
) |>
  mutate(
    n_cases = sum(cfr_data$cases),
    n_deaths = sum(cfr_data$deaths),
    ic = paste0(
      scales::percent(severity_low, accuracy = 0.1),
      " – ",
      scales::percent(severity_high, accuracy = 0.1)
    )
  ) |>
  select(method, n_cases, n_deaths, severity_estimate, ic)

cfr_gt <- cfr_tbl |>
  gt::gt() |>
  gt::cols_label(
    method = "Méthode",
    n_cases = "Cas",
    n_deaths = "Décès",
    severity_estimate = "Létalité (CFR)",
    ic = "IC 95 %"
  ) |>
  gt::fmt_percent(severity_estimate, decimals = 1) |>
  gt::cols_align(
    align = "center",
    columns = c(n_cases, n_deaths, severity_estimate, ic)
  ) |>
  gt::tab_source_note(
    gt::md(paste0("Données au ", fr_date(date_report)))
  )

cfr_gt |>
  save_gt("butembo_cfr.png")

#* 2. CFR over time — cumulative (rolling) ----------------------------
# running estimate as data accrue, crude vs delay-adjusted
rolling <- bind_rows(
  cfr::cfr_rolling(cfr_data) |> mutate(type = "Brute"),
  cfr::cfr_rolling(cfr_data, delay_density = delay_density) |>
    mutate(type = "Ajustée")
) |>
  mutate(type = factor(type, levels = c("Ajustée", "Brute")))

# skip the unstable early tail (few cumulative cases)
start_date <- cfr_data |>
  mutate(cum = cumsum(cases)) |>
  filter(cum >= 10) |>
  slice_min(date, n = 1) |>
  pull(date)

cfr_time_cols <- c("Ajustée" = "#bc5c5c", "Brute" = "#3a7ca5")

butembo_cfr_time <- rolling |>
  filter(date >= start_date) |>
  ggplot(aes(date, severity_estimate, colour = type, fill = type)) +
  geom_ribbon(
    aes(ymin = severity_low, ymax = severity_high),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 0.8) +
  scale_x_date(
    date_labels = "%d/%m",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    limits = c(0, 1),
    breaks = scales::breaks_width(0.2),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_colour_manual(values = cfr_time_cols) +
  scale_fill_manual(values = cfr_time_cols) +
  labs(
    x = NULL,
    y = "Létalité (CFR)",
    colour = NULL,
    fill = NULL,
    caption = paste0(
      "Estimation cumulée (rolling) — brute vs ajustée pour le délai début → décès\n",
      "Données au ",
      fr_date(date_report)
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey80",
      linewidth = 0.2,
      linetype = "62"
    ),
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_cfr_time

ggsave(
  fs::path(out_dir, "butembo_cfr_time.png"),
  butembo_cfr_time,
  height = 6,
  width = 8,
  dpi = 300,
  bg = "white"
)

#* 3. CFR by age group — crude (non ajustée) --------------------------
# crude proportion deaths / cases with an exact binomial (Clopper-Pearson) CI,
# computed directly rather than via cfr::cfr_static. The date-series builder
# counts cases only when onset is dated but counts every dated death, so fatal
# cases with a missing onset can make deaths > cases and the estimator errors
# out — dropping sparse strata like 0-9. Counting all cases in the denominator
# keeps every stratum valid.
age_levels <- levels(pos_data_clean$age_group)

cfr_by_age <- pos_data_clean |>
  filter(!is.na(age_group)) |>
  summarise(
    n_cases = dplyr::n(),
    n_deaths = sum(type_of_exit == "Décédé", na.rm = TRUE),
    .by = age_group
  ) |>
  mutate(
    cfr = n_deaths / n_cases,
    ci = purrr::map2(n_deaths, n_cases, ~ binom.test(.x, .y)$conf.int),
    low = purrr::map_dbl(ci, 1L),
    high = purrr::map_dbl(ci, 2L),
    age_group = factor(age_group, levels = age_levels)
  ) |>
  select(-ci) |>
  arrange(age_group)

# sample size on the x-axis labels
age_xlabs <- setNames(
  paste0(cfr_by_age$age_group, "\n(n = ", cfr_by_age$n_cases, ")"),
  as.character(cfr_by_age$age_group)
)

butembo_cfr_age <- cfr_by_age |>
  ggplot(aes(x = age_group, y = cfr)) +
  geom_errorbar(
    aes(ymin = low, ymax = high),
    colour = "#010101",
    alpha = 0.9, # much fainter CI
    linewidth = 0.3, # thinner CI
    width = .1,
  ) +
  geom_point(
    colour = "#bc5c5c",
    size = 3, # bigger point
    alpha = .9
  ) +
  scale_x_discrete(labels = age_xlabs) +
  scale_y_continuous(
    labels = scales::label_percent(),
    limits = c(0, 1),
    breaks = scales::breaks_width(0.2),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Groupe d'âge",
    y = "Létalité (CFR)",
    caption = paste0(
      "n = nombre de cas par groupe d'âge · létalité brute (non ajustée)\n",
      "Données au ",
      fr_date(date_report)
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey80",
      linewidth = 0.2,
      linetype = "62"
    ),
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    plot.margin = margin(10, 14, 10, 10)
  )

butembo_cfr_age

ggsave(
  fs::path(out_dir, "butembo_cfr_age.png"),
  butembo_cfr_age,
  height = 7,
  width = 9,
  dpi = 300,
  bg = "white"
)

#* MORTALITÉ (décès à l'arrivée & décès communautaire) -----------------
# n et % parmi les cas avec information renseignée
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
  gt::tab_source_note(
    gt::md(paste0("Données au ", fr_date(date_report)))
  )

mort_desc_gt |>
  save_gt("butembo_mortality_desc.png")
