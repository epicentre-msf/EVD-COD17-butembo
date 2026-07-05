# utils

# Format a date as the French validity label used in table/map footers,
# e.g. fr_date(as.Date("2026-06-23")) -> "23/06/2026".
fr_date <- function(date) {
  format(as.Date(date), "%d/%m/%Y")
}

# Latest data date per health zone, returned as a named Date vector
# (names = zone). Zones (e.g. Butembo, Katwa) are often updated on different
# days, so map captions need each zone's own validity date rather than a single
# global max. Feed the result to tm_theme_epi(date = ...).
# e.g. zone_last_dates(alert) -> c(Butembo = "2026-06-28", Katwa = "2026-07-01")
zone_last_dates <- function(
  df,
  zone_col = "adm2_name",
  date_col = "sitrep_date"
) {
  zone <- as.character(df[[zone_col]])
  date <- as.Date(df[[date_col]])
  agg <- tapply(date, zone, max) # numeric (days since epoch), named by zone
  out <- as.Date(agg, origin = "1970-01-01")
  out[order(names(out))]
}

# Save a gt table to out_dir as a high-resolution PNG.
# gtsave crops tight to the table, so width follows the table's own content;
# zoom is the resolution multiplier (higher = crisper, larger file).
# font_size sets a consistent (small) text size across all tables.
#
# gtsave bakes `zoom`x pixels into the PNG but tags it at 72 dpi, so Word /
# Quarto treat every table as `zoom` times its true physical size — hence the
# need for hand-tuned out-width per table and the inconsistent apparent scale.
# We rewrite the real resolution (72 * zoom) so the physical size is correct and
# all tables render at one consistent text scale without per-chunk out-width.
save_gt <- function(gt_tbl, file, zoom = 3, font_size = 11) {
  path <- fs::path(out_dir, file)
  gt_tbl |>
    gt::tab_options(table.font.size = gt::px(font_size)) |>
    gt::gtsave(
      path,
      zoom = zoom,
      expand = 10
    )
  png::writePNG(png::readPNG(path), path, dpi = 72 * zoom)
  invisible(path)
}


get_admin_level_sp <- function(
  country,
  level,
  sp_path = Sys.getenv("SHAREPOINT_PATH")
) {
  require("sf")
  sp_dir <- fs::path(sp_path, "OutbreakTools - GeoBase", country)
  # get latest version directory
  latest <- max(fs::dir_ls(
    sp_dir,
    regexp = glue::glue("{country}__"),
    type = "directory"
  ))
  shp_path <- fs::path(
    latest,
    "sf",
    paste(country, tolower(level), sep = "_"),
    ext = "rds"
  )
  if (file.exists(shp_path)) {
    sf_out <- readr::read_rds(shp_path)
    if (inherits(sf_out, "sf")) {
      sf_out
    }
  }
}

# get ref for shp
get_country_ref_sp <- function(
  country,
  reactable = FALSE,
  sp_path = Sys.getenv("SHAREPOINT_PATH")
) {
  sp_dir <- fs::path(sp_path, "OutbreakTools - GeoBase", country)
  # get latest version directory
  latest <- max(fs::dir_ls(
    sp_dir,
    regexp = glue::glue("{country}__"),
    type = "directory"
  ))
  ref <- readr::read_rds(fs::path(
    latest,
    glue::glue("adm_reference_{country}"),
    ext = "rds"
  ))
  if (reactable) {
    rlang::check_installed("reactable")
    reactable::reactable(
      dplyr::select(ref, -dplyr::starts_with("adm0")),
      searchable = TRUE,
      filterable = TRUE,
      compact = TRUE,
      highlight = TRUE,
      elementId = "geo-ref-tbl",
      defaultColDef = reactable::colDef(
        format = reactable::colFormat(
          digits = 0,
          separators = TRUE,
          locales = "fr-FR"
        )
      ),
      columns = list(
        level = reactable::colDef(filterInput = rctbl_filter),
        adm1_name = reactable::colDef(filterInput = rctbl_filter)
      )
    )
  } else {
    ref
  }
}

time_stamp <- function() {
  format(Sys.time(), "%Y%m%d")
}

# ggplot axis labeller: format week-start Dates as French ISO (Monday-start)
# epiweek labels without the year, e.g. label_epiweek(as.Date("2019-05-13")) ->
# "S20". Pass to the `labels` argument of scale_x_date on weekly epicurves so
# ticks read as epiweeks while bars stay positioned on a continuous date axis.
label_epiweek <- function(x) {
  wk <- aweek::date2week(x, week_start = 1, floor_day = TRUE)
  sub("^\\d{4}-W", "S", as.character(wk))
}
