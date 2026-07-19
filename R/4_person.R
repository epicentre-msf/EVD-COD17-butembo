# ! Script of the PERSON situation in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)

#* PERSON (Age & sex pyramid) ------------------------------------------

# quick tabulation of cases by age group
pos_data_clean |>
  count(age_group, .drop = FALSE) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print()

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
  height = 6,
  width = 9,
  dpi = 300,
  bg = "white"
)

#* Cases by age group (table) ------------------------------------------
inf_age_tbl <- pos_data_clean |>
  filter(!is.na(age_group)) |>
  count(age_group, .drop = FALSE) |>
  arrange(age_group) |>
  mutate(
    pct = n / sum(n),
    n_pct = paste0(n, " (", scales::percent(pct, accuracy = 0.1), ")")
  ) |>
  select(age_group, n_pct)

inf_age_gt <- inf_age_tbl |>
  gt::gt(rowname_col = "age_group") |>
  gt::tab_stubhead(label = "Groupe d'âge") |>
  gt::cols_label(n_pct = "N (%)") |>
  gt::cols_align(align = "center", columns = n_pct) |>
  gt::tab_source_note(
    gt::md(paste0("Données au ", fr_date(date_report)))
  )

inf_age_gt |>
  save_gt("butembo_infection_age.png")

#* Cases by age group (reactable) --------------------------------------
case_ramp <- c("#ffffff", "#fd7e14")

ramp_style <- function(ramp, domain) {
  pal <- scales::colour_ramp(ramp)
  function(value) {
    if (is.null(value) || is.na(value)) {
      return(list())
    }
    frac <- max(0, min(1, (value - domain[1]) / (domain[2] - domain[1])))
    list(background = pal(frac))
  }
}

age_rctbl_data <- pos_data_clean |>
  filter(!is.na(age_group)) |>
  count(age_group, .drop = FALSE) |>
  arrange(age_group) |>
  mutate(
    pct = n / sum(n),
    n_pct = paste0(n, " (", scales::percent(pct, accuracy = 0.1), ")")
  )

age_reactable <- reactable::reactable(
  age_rctbl_data[c("age_group", "n", "n_pct")],
  highlight = TRUE,
  compact = TRUE,
  striped = FALSE,
  pagination = FALSE,
  theme = reactable::reactableTheme(
    style = list(fontSize = "0.82rem"),
    headerStyle = list(fontSize = "0.78rem", fontWeight = 600),
    footerStyle = list(fontWeight = 600),
    cellPadding = "4px 6px"
  ),
  defaultColDef = reactable::colDef(align = "center", minWidth = 60),
  columns = list(
    age_group = reactable::colDef(
      name = "Groupe d'âge",
      align = "left",
      sticky = "left",
      footer = "Total"
    ),
    n = reactable::colDef(
      name = "N (%)",
      style = ramp_style(case_ramp, c(0, max(age_rctbl_data$n))),
      cell = function(value, index) age_rctbl_data$n_pct[index],
      footer = sum(age_rctbl_data$n)
    ),
    n_pct = reactable::colDef(show = FALSE)
  )
)

age_panel <- htmltools::browsable(htmltools::div(
  class = "age-summary",
  style = "font-family: sans-serif; max-width: 420px; border: 1px solid #dee2e6; border-radius: 6px; padding: 14px 18px;",
  htmltools::h2(
    "Cas confirmés par groupe d'âge",
    style = "margin: 0; font-size: 1.25rem;"
  ),
  htmltools::div(
    paste0("Données au ", fr_date(date_report)),
    style = "color: #6c757d; font-size: 0.85rem; margin-bottom: 14px;"
  ),
  age_reactable
))

age_panel

age_panel |>
  save_widget("butembo_infection_age_reactable.png", selector = ".age-summary")
