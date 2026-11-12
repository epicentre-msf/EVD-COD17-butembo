# Ad hoc analysis of alerts for the Butembo EVD outbreak.
# Reads the latest clean linelist; scratch script, not part of the pipeline.

#* TODO ------------------------------------
# - [ ] define the alert source and the question to answer

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))

#* Path ------------------------------------

alert_path <- fs::path(butembo_project_data_path, "brute", "alertes")

latest_mus_alert <- fs::dir_ls(fs::path(alert_path, "musienene")) |> max()

#* Import data -----------------------------

# Mus alerts are in a wide format
mus_alert <- rio::import(latest_mus_alert, which = "data", skip = 1) |>
  as_tibble() |>
  clean_names()

# katwa data are in a linelist format
kat_alert <- rio

#* Rename variables ------------------------
# Excel headers are positional leftovers; all_of() errors if the layout shifts.
mus_alert_names <- c(
  date = "date",
  health_zone = "zone_sante_2",
  health_area = "aire_sante",
  alert_live = "v4",
  alert_dead = "d5",
  investigated_live = "v6",
  investigated_dead = "d7",
  valid_live = "v10",
  valid_dead = "d11",
  invalid_live = "v12",
  invalid_dead = "d13",
  sampled = "prelev",
  alert_source_fosa = "fosa",
  alert_source_ra = "ra",
  alert_source_comm = "communaute",
  alert_source_poc = "po_c"
)
mus_alert <- mus_alert |>
  select(all_of(mus_alert_names))

cat("mus_alert:", nrow(mus_alert), "rows,", ncol(mus_alert), "cols\n")

#* DATA CLEANING ---------------------------------
mus_alert <- mus_alert |>
  mutate(
    date = ymd(date),
    week_date = floor_date(date, unit = "week", week_start = 1),
    week = aweek::date2week(date, floor_day = 1),
    health_zone = tolower(health_zone),
    health_area = tolower(str_squish(health_area))
  ) |>
  relocate(c(week, week_date), .after = date)

#* long formats
alert_stage_long <- mus_alert |>
  select(week_date, health_zone, health_area, matches("_(live|dead)$")) |>
  filter(!is.na(health_area)) |>
  pivot_longer(
    matches("_(live|dead)$"),
    names_to = c("stage", "status"),
    names_sep = "_",
    values_to = "n"
  )

alert_source_long <- mus_alert |>
  select(week_date, health_zone, starts_with("alert_source_")) |>
  pivot_longer(
    starts_with("alert_source_"),
    names_to = "alert_source",
    names_prefix = "alert_source_",
    values_to = "n"
  )

#* dfs

#* global / week - % valid
df_alert_week_live <- alert_stage_long |>
  filter(stage == "alert") |>
  mutate(
    .by = c(week_date, health_zone),
    tot_alert = sum(n),
    pct_live = sum(n[status == "live"]) / tot_alert
  )

#* global / week - % valid
df_alert_week_valid <- alert_stage_long |>
  filter(stage %in% c("alert", "valid")) |>
  summarise(
    .by = c(week_date, health_zone, stage),
    n = sum(n)
  ) |>
  mutate(
    .by = c(week_date, health_zone),
    tot_alert = sum(n[stage == "alert"]),
    pct_valid = sum(n[stage == "valid"]) / tot_alert
  )

#* global / week - sources

df_alert_week_source <- alert_source_long |>
  filter(!is.na(health_area)) |>
  summarise(.by = c(week_date, health_zone, alert_source), n = sum(n))


#* Plot helpers ----------------------------
# Copied from evd-2026-app/R/theme_epihelpers.R; keep in sync.
theme_epihelpers <- function(base_size = 11, ...) {
  theme_light(base_size) +
    theme(
      axis.line.x.bottom = element_blank(),

      ## axis title size and face
      axis.title = element_text(size = 16, face = "bold"),

      legend.text = element_text(size = 12),

      ## remove the grey background in legend
      legend.key = element_blank(),

      ## remove y -axis lines
      axis.line.y = element_blank(),

      # background
      panel.grid.major = element_line(colour = "grey", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", colour = NA),

      # caption (italics, on left side)
      plot.caption = element_text(hjust = 0, size = 10),

      ## facet appearance
      strip.text.x = element_text(size = 18, face = "bold"),
      strip.text.y = element_text(size = 18, face = "bold", hjust = 0),
      panel.spacing.x = unit(1, "lines"),

      plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),

      ...
    )
}

geom_col_epihelpers <- function(width = 1, ...) {
  args <- utils::modifyList(
    list(colour = "grey70", linewidth = 0.2, width = width),
    list(...)
  )
  do.call(ggplot2::geom_col, args)
}

#* Weekly totals with % ---------------------
# Summed across zones; pct repeats per row so the line can be drawn once per week.
weekly_source <- df_alert_week_source |>
  summarise(.by = c(week_date, alert_source), n = sum(n)) |>
  mutate(.by = week_date, pct = n[alert_source == "comm"] / sum(n))

weekly_valid <- df_alert_week_valid |>
  summarise(.by = c(week_date, stage), n = sum(n)) |>
  mutate(.by = week_date, pct = n[stage == "valid"] / n[stage == "alert"]) |>
  arrange(week_date, stage)

weekly_live <- df_alert_week_live |>
  summarise(.by = c(week_date, status), n = sum(n)) |>
  mutate(.by = week_date, pct = n[status == "live"] / sum(n))

#* Plots

# The line is rescaled onto the count axis, with a % secondary axis to read it.
plot_alert_bars <- function(dat, fill, position = "stack") {
  bar_max <- if (position == "stack") {
    max(summarise(dat, .by = week_date, tot = sum(n))$tot)
  } else {
    max(dat$n)
  }
  pct_line <- dat |> distinct(week_date, pct)

  ggplot(dat, aes(x = week_date)) +
    geom_col_epihelpers(
      width = 7,
      position = position,
      mapping = aes(y = n, fill = {{ fill }})
    ) +
    geom_line(
      data = pct_line,
      mapping = aes(y = pct * bar_max),
      colour = "black",
      linewidth = 0.8
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        \(y) y / bar_max,
        name = "%",
        labels = scales::percent
      )
    ) +
    labs(x = "Epi Week", y = "Alerts", fill = NULL) +
    theme_epihelpers(base_size = 18)
}

# % of alerts from the community (assumed question; see TODO).
plot_alert_bars(weekly_source, alert_source)

# Valid alerts sit inside all alerts, so overlay rather than stack.
plot_alert_bars(weekly_valid, stage, position = "identity")

plot_alert_bars(weekly_live, status)
