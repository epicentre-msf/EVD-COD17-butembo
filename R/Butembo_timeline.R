# Patient timelines: date of symptom onset -> exit (recovery / death)
# Interactive xrange chart; tooltip shows the disease phase, onset/exit and
# each health structure visited with its dates.

source(here::here("R", "0_global.R"))
library(highcharter)

ll <- readRDS(latest_narr_ll_clean)$data

# disease phase: same light red for every case
disease_col <- "#f2b0b0"

# a few resolved cases to start with
set.seed(17)

tl_dat <- ll |>
  filter(
    type_of_exit %in% c("Guéri", "Décédé"),
    !is.na(date_symptom_onset),
    !is.na(date_exit_eff)
  ) |>
  slice_sample(n = 8) |>
  mutate(
    sex_age = paste0(if_else(sex == "Homme", "M", "F"), age),
    patient_label = glue::glue("{patient_name} ({sex_age})"),
    patient_label = forcats::fct_reorder(patient_label, date_symptom_onset),
    # each day fills the cell between two grid lines, so span to the next midnight
    onset_end = date_symptom_onset + 1,
    exit_end = date_exit_eff + 1
  )

# health structures visited by the sampled patients (one row per visit)
hf_visits <- ll |>
  select(
    patient_name,
    contains("HF_name_visited"),
    contains("date_start_HF_visited"),
    contains("date_end_HF_visited")
  ) |>
  mutate(across(-patient_name, as.character)) |>
  pivot_longer(
    cols = -patient_name,
    names_to = c(".value", "visit"),
    names_pattern = "(.*?)(\\d+)$"
  ) |>
  rename_with(\(x) str_remove(x, "_$")) |>
  mutate(across(where(is.character), \(x) na_if(str_squish(x), ""))) |>
  filter(!is.na(HF_name_visited)) |>
  mutate(
    hf_name = str_squish(str_split_i(HF_name_visited, fixed("|"), 1)),
    # initials of each word, e.g. "CH La Guérison" -> "CLG"
    hf_abbr = map_chr(
      str_split(hf_name, "\\s+"),
      \(w) str_c(str_to_upper(str_sub(w, 1, 1)), collapse = "")
    ),
    across(c(date_start_HF_visited, date_end_HF_visited), as.Date)
  ) |>
  filter(!is.na(date_start_HF_visited)) |>
  inner_join(
    select(tl_dat, patient_name, patient_label, exit_end),
    by = "patient_name"
  ) |>
  mutate(
    is_last_visit = date_start_HF_visited == max(date_start_HF_visited),
    .by = patient_name
  ) |>
  # end date: recorded end if valid, else the exit for the last structure,
  # else a single day; +1 so the last day fills its cell
  mutate(
    hf_end = case_when(
      !is.na(date_end_HF_visited) &
        date_end_HF_visited >= date_start_HF_visited ~ date_end_HF_visited + 1,
      is_last_visit ~ exit_end,
      .default = date_start_HF_visited + 1
    )
  )

#* HIGHCHARTER TIMELINE --------------------------------------------------------

cats <- levels(tl_dat$patient_label)

# Date -> epoch ms (UTC so day boundaries don't shift)
to_ms <- function(d) as.numeric(as.POSIXct(as.Date(d), tz = "UTC")) * 1000
day_ms <- 24 * 3600 * 1000

# day-cell grid: gridlines at each midnight (boundaries), labels centred at noon
range_start <- min(c(
  tl_dat$date_symptom_onset,
  hf_visits$date_start_HF_visited
))
range_end <- max(c(tl_dat$exit_end, hf_visits$hf_end))
midnights <- seq(range_start, range_end, by = "day")
grid_lines <- lapply(
  to_ms(midnights),
  \(v) list(value = v, color = "#d9d9d9", width = 1, zIndex = 1)
)
day_labels <- to_ms(head(midnights, -1)) + day_ms / 2

# one point = one rectangle; y is the 0-based category index
bar_pts <- tl_dat |>
  transmute(
    y = as.integer(patient_label) - 1L,
    x = to_ms(date_symptom_onset),
    x2 = to_ms(exit_end),
    color = disease_col,
    outcome = as.character(type_of_exit),
    d_start = format(date_symptom_onset, "%d %b %Y"),
    d_end = format(date_exit_eff, "%d %b %Y")
  )

onset_pts <- tl_dat |>
  transmute(
    y = as.integer(patient_label) - 1L,
    x = to_ms(date_symptom_onset),
    x2 = to_ms(onset_end),
    color = "darkred",
    d_start = format(date_symptom_onset, "%d %b %Y")
  )

exit_pts <- tl_dat |>
  transmute(
    y = as.integer(patient_label) - 1L,
    x = to_ms(date_exit_eff),
    x2 = to_ms(exit_end),
    color = if_else(type_of_exit == "Guéri", "#add8e6", "#7f7f7f"),
    outcome = as.character(type_of_exit),
    d_end = format(date_exit_eff, "%d %b %Y")
  )

hf_pts <- hf_visits |>
  transmute(
    y = as.integer(patient_label) - 1L,
    x = to_ms(date_start_HF_visited),
    x2 = to_ms(hf_end),
    color = "rgba(0,0,0,0)",
    borderColor = "#262626",
    hf_name = hf_name,
    abbr = hf_abbr,
    d_start = format(date_start_HF_visited, "%d %b %Y"),
    d_end = format(hf_end - 1, "%d %b %Y")
  )

butembo_timeline_hc <- highchart() |>
  hc_chart(type = "xrange") |>
  hc_xAxis(
    type = "datetime",
    opposite = TRUE,
    gridLineWidth = 0,
    tickLength = 0,
    lineWidth = 0,
    min = to_ms(range_start),
    max = to_ms(range_end),
    startOnTick = FALSE,
    endOnTick = FALSE,
    minPadding = 0,
    maxPadding = 0,
    tickPositions = day_labels,
    labels = list(format = "{value:%d %b}"),
    plotLines = grid_lines
  ) |>
  hc_yAxis(categories = cats, reversed = TRUE, title = list(text = NULL)) |>
  # grouping = FALSE overlays the series on one row instead of stacking them
  hc_plotOptions(
    xrange = list(pointWidth = 16, borderRadius = 0, grouping = FALSE)
  ) |>
  hc_add_series(
    name = "Maladie",
    data = list_parse(bar_pts),
    tooltip = list(
      headerFormat = "",
      pointFormat = "<b>Maladie ({point.outcome})</b><br>Début : {point.d_start}<br>Sortie : {point.d_end}"
    )
  ) |>
  hc_add_series(
    name = "Structures",
    data = list_parse(hf_pts),
    dataLabels = list(enabled = TRUE, format = "{point.abbr}"),
    tooltip = list(
      headerFormat = "",
      pointFormat = "<b>{point.hf_name}</b><br>Du {point.d_start} au {point.d_end}"
    )
  ) |>
  # onset / exit added last so they stay on top of the structure rectangles
  hc_add_series(
    name = "Début des symptômes",
    data = list_parse(onset_pts),
    showInLegend = FALSE,
    tooltip = list(
      headerFormat = "",
      pointFormat = "<b>Début des symptômes</b><br>{point.d_start}"
    )
  ) |>
  hc_add_series(
    name = "Sortie",
    data = list_parse(exit_pts),
    showInLegend = FALSE,
    tooltip = list(
      headerFormat = "",
      pointFormat = "<b>Sortie ({point.outcome})</b><br>{point.d_end}"
    )
  ) |>
  hc_legend(enabled = FALSE)

butembo_timeline_hc
