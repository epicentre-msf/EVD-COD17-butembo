# Helpers for the health-facility analyses, shared by the dashboard module and
# R/10_health-facilitiesf.R. Kept here as the dashboard only sees its own folder.

# CT/CTE receive confirmed cases, so sit outside the pre-isolation care pathway
HF_CTE_EXCLUDE <- c("HGR Kitatumba", "HGR Katwa", "HGR Matanda")

# table name -> exact name in the FOSA layer
HF_MANUAL <- c("UCG" = "Cliniques Universitaires du Graben")

norm_hf <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    stringr::str_squish()
}

# drops the type prefix (CH, CS, Disp, HGR...) so only the proper name is compared
strip_hf_type <- function(x) {
  stringr::str_squish(stringr::str_remove(
    x,
    "^(ch|cs|cm|csr|ps|disp|dispensaire|centre hospitalier|centre de sante|centre medico naturel|hgr|hopital general de reference|hop|poste de sante|clinique|cte|ct)\\b"
  ))
}

# FOSA layer with the normalised keys used for matching
hf_prepare_ref <- function(hf) {
  hf |>
    dplyr::mutate(
      core = strip_hf_type(norm_hf(dplyr::coalesce(short_name, name))),
      core_full = strip_hf_type(norm_hf(name)),
      as_n = norm_hf(adm3_name)
    )
}

# row of `hf_ref` matching a structure within its health area (NA if none)
hf_match_idx <- function(hf_name, hf_as, hf_ref) {
  if (hf_name %in% names(HF_MANUAL)) {
    hit <- which(hf_ref$name == HF_MANUAL[[hf_name]])
    same <- hit[which(hf_ref$as_n[hit] == norm_hf(hf_as))]
    if (length(same) > 0) {
      hit <- same
    }
    return(hit[1])
  }
  cand <- which(hf_ref$as_n == norm_hf(hf_as))
  if (length(cand) == 0) {
    return(NA_integer_)
  }
  core <- strip_hf_type(norm_hf(hf_name))
  d <- pmin(
    stringdist::stringdist(core, hf_ref$core[cand], method = "jw", p = 0.1),
    stringdist::stringdist(core, hf_ref$core_full[cand], method = "jw", p = 0.1)
  )
  if (min(d) > 0.15) {
    return(NA_integer_)
  }
  cand[which.min(d)]
}

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
