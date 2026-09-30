# Helpers for the health-facility analyses, shared by the dashboard module and
# R/10_health-facilitiesf.R. Kept here as the dashboard only sees its own folder.
# Name matching to the FOSA layer lives in R/utils.R and runs in prep.

# CT/CTE receive confirmed cases, so sit outside the pre-isolation care pathway
HF_CTE_EXCLUDE <- c("HGR Kitatumba", "HGR Katwa", "HGR Matanda")

# cases who started a visit in the `n` days up to and including `anchor`
hf_n_seen <- function(patient, date, anchor, n) {
  idx <- !is.na(date) & date >= anchor - (n - 1) & date <= anchor
  dplyr::n_distinct(patient[idx])
}

# Structures ranked by cases seen; 7/14/21 d windows are cumulative.
hf_top_structures <- function(
  visits,
  anchor,
  id_col = "unique_id",
  exclude = HF_CTE_EXCLUDE,
  n = 15
) {
  visits |>
    dplyr::filter(!is.na(hf_name), !hf_name %in% exclude) |>
    dplyr::summarise(
      .by = c(hf_name, hf_as, hf_zs),
      total = dplyr::n_distinct(.data[[id_col]]),
      j7 = hf_n_seen(.data[[id_col]], date_start_HF_visited, anchor, 7),
      j14 = hf_n_seen(.data[[id_col]], date_start_HF_visited, anchor, 14),
      j21 = hf_n_seen(.data[[id_col]], date_start_HF_visited, anchor, 21)
    ) |>
    dplyr::filter(j21 > 0) |>
    dplyr::arrange(dplyr::desc(total), dplyr::desc(j21), dplyr::desc(j14), dplyr::desc(j7)) |>
    dplyr::slice_head(n = n)
}

# cell background from a colour ramp, for reactable count columns
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
