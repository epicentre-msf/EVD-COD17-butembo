# ! Script of the patient HF travels

source(here::here("R", "0_global.R"))

# import the narrative linelist
latest_narr_ll <- fs::dir_ls(fs::path(
  butembo_project_data_path,
  "LL-epic",
  "narrative LL",
  "export_ll"
)) |>
  max()

ll_narr <- rpxl::rp_xlsb(latest_narr_ll, password = "ebolaExport", sheet = 1) |>
  as_tibble() |>
  remove_empty()


# Select var
hf_visits <- ll_narr |>
  select(
    patient_name,
    contains("HF_name_visited"),
    contains("date_start_HF_visited"),
    contains("date_end_HF_visited")
  ) |>
  pivot_longer(
    cols = !patient_name,
    names_to = c(".value", "visit"),
    names_pattern = "(.*?)(\\d+)$",
    names_transform = list(visit = as.integer)
  ) |>
  # strip any trailing underscore left on the value column names
  rename_with(\(x) str_remove(x, "_$")) |>
  # turn empty strings into NA
  mutate(across(where(is.character), \(x) na_if(x, ""))) |>
  filter(if_all(
    c(HF_name_visited, date_start_HF_visited, date_end_HF_visited),
    ~ !is.na(.x)
  )) |>
  # make sure the dates are Date class
  mutate(across(c(date_start_HF_visited, date_end_HF_visited), as.Date)) |>
  # data correction: Donatien date entered as 2030-05-26 instead of 2026-05-30
  mutate(across(
    c(date_start_HF_visited, date_end_HF_visited),
    \(x) {
      if_else(
        str_detect(patient_name, regex("donatien", ignore_case = TRUE)) &
          x == as.Date("2030-05-26"),
        as.Date("2026-05-30"),
        x
      )
    }
  ))

# Per-patient event dates (onset / notification / exit), overlaid as points
hf_events <- ll_narr |>
  select(
    patient_name,
    date_symptom_onset,
    date_notification,
    date_exit_eff,
    type_of_exit
  ) |>
  # date_exit_eff comes in as a raw Excel serial number (numeric), so it needs
  # the Excel origin (1899-12-30); as.Date() alone would shift it to ~2096.
  # The other two columns are already Date class.
  mutate(
    across(c(date_symptom_onset, date_notification), as.Date),
    date_exit_eff = janitor::excel_numeric_to_date(as.numeric(date_exit_eff))
  ) |>
  # keep only the patients shown in the HF-visit plot
  filter(patient_name %in% hf_visits$patient_name) |>
  # exit is black if dead, blue if alive
  mutate(
    exit_event = if_else(
      type_of_exit == "Décédé",
      "Exit (dead)",
      "Exit (alive)"
    )
  ) |>
  pivot_longer(
    c(date_symptom_onset, date_notification, date_exit_eff),
    names_to = "event",
    values_to = "date"
  ) |>
  filter(!is.na(date)) |>
  mutate(
    event = case_when(
      event == "date_symptom_onset" ~ "Symptom onset",
      event == "date_notification" ~ "Notification",
      event == "date_exit_eff" ~ exit_event
    ),
    event = factor(
      event,
      levels = c("Symptom onset", "Notification", "Exit (alive)", "Exit (dead)")
    )
  )

event_colours <- c(
  "Symptom onset" = "red",
  "Notification" = "yellow",
  "Exit (alive)" = "lightblue",
  "Exit (dead)" = "black"
)

# Probable incubation window: the 10 days preceding symptom onset (DSO).
# patient_name is set as a factor with the same level order as the y-axis
# (driven by hf_visits) so the rectangles line up with the right rows.
incubation <- hf_events |>
  filter(event == "Symptom onset") |>
  distinct(patient_name, date) |>
  mutate(
    incub_start = date - 10,
    incub_end = date,
    patient_name = factor(
      patient_name,
      levels = sort(unique(hf_visits$patient_name))
    )
  )


# Interactive Gantt-style chart (highcharter) --------------------------------
library(highcharter)

# Shared y-axis category order: patients sorted by symptom-onset date so the
# earliest case sits at the top (y-axis is reversed below). Patients with no
# recorded onset fall to the bottom; ties broken alphabetically.
onset_by_patient <- hf_events |>
  filter(event == "Symptom onset") |>
  group_by(patient_name) |>
  summarise(onset = min(date), .groups = "drop")

patient_levels <- tibble(patient_name = unique(hf_visits$patient_name)) |>
  left_join(onset_by_patient, by = "patient_name") |>
  arrange(onset, patient_name) |>
  pull(patient_name)
# Highcharts categories are 0-indexed.
y_index <- function(p) match(as.character(p), patient_levels) - 1L

fmt_range <- function(start, end) {
  paste(format(start, "%d %b %Y"), "&rarr;", format(end, "%d %b %Y"))
}

# One consistent colour per facility, shared across the whole chart.
# Any facility whose name contains "inconnu" (unknown) is greyed out.
facilities <- sort(unique(hf_visits$HF_name_visited))
named <- facilities[
  !str_detect(facilities, regex("inconnu", ignore_case = TRUE))
]
# Qualitative palette so facilities are maximally distinct (viridis is
# sequential and gives near-identical neighbours). Polychrome 36 holds up to
# 36 distinct hues; ramp across them if there are ever more facilities.
n <- length(named)
pal <- if (n <= 36) {
  unname(grDevices::palette.colors(n, palette = "Polychrome 36"))
} else {
  grDevices::colorRampPalette(grDevices::palette.colors(36, "Polychrome 36"))(n)
}
facility_colours <- setNames(pal, named)
facility_colours[setdiff(facilities, named)] <- "lightgrey"

# HF visits as an xrange series (one bar per visit, coloured by facility name)
visits_data <- hf_visits |>
  transmute(
    x = datetime_to_timestamp(date_start_HF_visited),
    x2 = datetime_to_timestamp(date_end_HF_visited),
    y = y_index(patient_name),
    color = unname(facility_colours[HF_name_visited]),
    facility = HF_name_visited,
    date_range = fmt_range(date_start_HF_visited, date_end_HF_visited)
  ) |>
  list_parse()

# Probable incubation windows as a faint xrange series, drawn behind the visits
incub_data <- incubation |>
  transmute(
    x = datetime_to_timestamp(incub_start),
    x2 = datetime_to_timestamp(incub_end),
    y = y_index(patient_name),
    date_range = fmt_range(incub_start, incub_end)
  ) |>
  list_parse()

hc <- highchart() |>
  hc_chart(type = "xrange") |>
  hc_title(text = "Health facilities visited by patient over time") |>
  hc_xAxis(type = "datetime", title = list(text = NULL)) |>
  hc_yAxis(
    categories = patient_levels,
    reversed = TRUE,
    title = list(text = NULL)
  ) |>
  # xrange/column series are grouped (offset side-by-side) by default, which
  # pushes each series off its patient row; grouping = FALSE overlaps them so
  # every bar lines up with its patient name.
  hc_plotOptions(series = list(grouping = FALSE)) |>
  hc_add_series(
    name = "Probable incubation",
    data = incub_data,
    type = "xrange",
    color = "rgba(255, 248, 196, 0.45)",
    colorByPoint = FALSE,
    pointWidth = 14,
    borderColor = "transparent",
    tooltip = list(
      headerFormat = "",
      pointFormat = "<b>Probable incubation</b><br/>{point.date_range}"
    )
  ) |>
  hc_add_series(
    name = "HF visit",
    data = visits_data,
    type = "xrange",
    pointWidth = 14,
    borderColor = "white",
    showInLegend = FALSE,
    tooltip = list(
      headerFormat = "",
      pointFormat = "<b>{point.facility}</b><br/>{point.date_range}"
    )
  )

# Overlay the per-patient events as points, one series per type
for (ev in levels(hf_events$event)) {
  ev_data <- hf_events |>
    filter(event == ev) |>
    transmute(
      x = datetime_to_timestamp(date),
      y = y_index(patient_name)
    ) |>
    list_parse()

  if (length(ev_data) == 0) {
    next
  }

  hc <- hc |>
    hc_add_series(
      name = ev,
      data = ev_data,
      type = "scatter",
      color = unname(event_colours[ev]),
      marker = list(radius = 5, symbol = "circle"),
      enableMouseTracking = FALSE,
      dataLabels = list(enabled = FALSE)
    )
}

hc <- hc |>
  hc_tooltip(useHTML = TRUE) |>
  hc_legend(enabled = TRUE)

hc
