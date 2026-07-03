# ! Script of the ETC situation in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)


#* SYMPTOMS ------------------------------------------------
# Symptom comparison of Confirmed cases vs Non-cases in the NK linelist,
# replicating R/symptoms/symptoms_analysis.R for the Butembo / Katwa data.
# (The delay-to-admission temporal panels are omitted here: admission dates are
# largely missing and n = 16 confirmed is too sparse to stratify.)

triage <- c("bleeding_gum", "bleeding_injection_site", "conjunctivitis")

# Shared plot aesthetics: Confirmed = dark red, Non-case = blue
status_cols <- c(
  "Confirmé" = "darkred",
  "Non cas" = "#6d85b6"
)

dodge <- position_dodge(width = 0.4)

# subset + harmonise the status labels with the symptoms_analysis.R convention
# ("Not a case" -> "Non-case"); NK uses "Suspected" rather than "Suspect".
nk_symp <- nk_ll |>
  select(EVD_status, all_of(symptoms))

# ! Missing values ------------------------------------
nk_symp |>
  select(all_of(symptoms)) |>
  epivis::plot_miss_vis()

# keep only Confirmed and Non-case for the comparison
nk_symp <- nk_symp |>
  filter(EVD_status %in% c("Non cas", "Confirmé"))

# * Symptoms table (count & prop per EVD status) -------------------------------

# Variables shown in the figure/table: general symptoms + the umbrella
# "bleeding", plus the triage bleeding vars (bleeding_gum, bleeding_injection_site).
global_vars <- symptoms[!(symptoms %in% bleeding_var) | symptoms %in% triage]

nk_symptoms_tbl <- nk_symp |>
  select(EVD_status, all_of(global_vars)) |>
  # force No/Yes levels so "Yes" is valid even where a symptom has no Yes
  mutate(across(all_of(global_vars), ~ factor(.x, levels = c("Non", "Oui")))) |>
  gtsummary::tbl_summary(
    by = EVD_status,
    type = all_of(global_vars) ~ "dichotomous",
    value = all_of(global_vars) ~ "Oui",
    statistic = all_categorical() ~ "{n} ({p})",
    missing = "no",
    label = as.list(all_labels[global_vars])
  ) |>
  gtsummary::add_overall() |>
  # p-value comparing Non-case vs Confirmed (chi-square, or Fisher for sparse cells)
  gtsummary::add_p() |>
  gtsummary::modify_header(
    label = "**Symptom**",
    all_stat_cols() ~ "**{level}**, N = {n} <br> n (%)"
  ) |>
  gtsummary::bold_labels()

nk_symptoms_tbl

# minimal styling for export: drop footnotes, strip inner rules, compact rows
nk_symptoms_gt <- nk_symptoms_tbl |>
  gtsummary::as_gt() |>
  gt::rm_footnotes() |>
  gt::tab_options(
    table.font.size = gt::px(12),
    data_row.padding = gt::px(2),
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.border.bottom.style = "none",
    table_body.hlines.style = "none"
  ) |>
  # faint-yellow highlight where the p-value is significant (< 0.05)
  gt::tab_style(
    style = gt::cell_fill(color = "#FFF6B0"),
    locations = gt::cells_body(columns = p.value, rows = p.value < 0.05)
  )

gt::gtsave(
  nk_symptoms_gt,
  fs::path(nk_output_path, "nk_symptoms_table.png"),
  zoom = 4,
  expand = 5
)

# ! Proportions chart -----------------------------------------------------------

nk_long <- nk_symp |>
  select(EVD_status, all_of(global_vars)) |>
  tidyr::pivot_longer(
    cols = everything() & !EVD_status,
    names_to = "group",
    values_to = "level"
  ) |>
  # keep missing as an explicit category so EVERY symptom shows Yes / No /
  # Missing (and missings count toward the denominator).
  mutate(
    level = level |>
      factor(levels = c("Non", "Oui")) |>
      forcats::fct_na_value_to_level("Missing")
  ) |>
  # .drop = FALSE keeps empty factor levels, so a symptom with zero "Yes" in a
  # status still gets a Yes row (n = 0, prop = 0)
  dplyr::count(EVD_status, group, level, name = "n", .drop = FALSE) |>
  arrange(group, EVD_status) |>
  dplyr::mutate(
    .by = c(EVD_status, group),
    total = sum(n),
    prop = n / total,
    prop_lab = round(prop, digits = 2)
  ) |>
  # Wilson 95% binomial CI for each proportion (n out of total)
  dplyr::mutate(
    .z = stats::qnorm(0.975),
    ci_low = (prop +
      .z^2 / (2 * total) -
      .z * sqrt((prop * (1 - prop) + .z^2 / (4 * total)) / total)) /
      (1 + .z^2 / total),
    ci_high = (prop +
      .z^2 / (2 * total) +
      .z * sqrt((prop * (1 - prop) + .z^2 / (4 * total)) / total)) /
      (1 + .z^2 / total),
    .z = NULL
  )

# rank symptoms by the proportion of "Yes" among Confirmed cases.
# start from all symptoms so those with zero "Yes" still get a rank (0)
order_vec <- tibble::tibble(group = symptoms) |>
  dplyr::left_join(
    nk_long |>
      dplyr::filter(level == "Oui", EVD_status == "Confirmé") |>
      dplyr::select(group, prop),
    by = "group"
  ) |>
  dplyr::mutate(prop = tidyr::replace_na(prop, 0)) |>
  dplyr::arrange(prop) |>
  dplyr::pull(group)

# bold the triage-algorithm symptoms on the y axis (ordered like the y breaks)
plotted_groups <- order_vec[order_vec %in% nk_long$group]
y_face <- ifelse(plotted_groups %in% triage, "bold", "plain")

# case counts per status (the per-symptom denominators), for the caption
n_by_status <- table(nk_symp$EVD_status)
plot_caption <- paste0(
  "Confirmé: ",
  n_by_status[["Confirmé"]],
  " cases  |  ",
  "Non cas: ",
  n_by_status[["Non cas"]]
)

nk_symptoms_plot <- nk_long |>
  dplyr::filter(level == "Oui") |>
  dplyr::mutate(
    group = factor(group, levels = order_vec),
    EVD_status = factor(EVD_status, levels = names(status_cols))
  ) |>
  ggplot(aes(x = prop, y = group, colour = EVD_status)) +
  geom_errorbarh(
    aes(xmin = ci_low, xmax = ci_high),
    height = 0.1,
    linewidth = .2,
    position = dodge,
    alpha = .4
  ) +
  geom_point(position = dodge, size = 2) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_discrete(labels = all_labels) +
  scale_colour_manual(values = status_cols) +
  labs(
    x = "Proportion with symptom",
    y = NULL,
    colour = NULL,
    caption = plot_caption
  ) +
  theme_evd(legend_position = "bottom", strip_text_size = 14) +
  theme(
    panel.grid.major.x = element_line(colour = "grey90"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.ticks.x = element_line(colour = "black"),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(face = y_face),
    legend.text = element_text(size = 10)
  )

nk_symptoms_plot

ggsave(
  fs::path(nk_output_path, "nk_symptoms_overall.png"),
  nk_symptoms_plot,
  height = 8,
  width = 9
)
