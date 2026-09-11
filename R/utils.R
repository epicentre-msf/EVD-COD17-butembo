# utils

#* EXPORTING ------------------------------

# the dir_ls regexps in 0_global.R match on this
export_prefix <- "BUT-EVD_BUTEMBO"

# stamp is passed in, so every dataset of one run carries the same one.
# dir is created first: on synced storage it may not be there yet
export_clean <- function(x, name, stamp, dir = butembo_project_clean_data_path) {
  fs::dir_create(dir)
  saveRDS(
    x,
    fs::path(dir, glue::glue("{export_prefix}_{name}__{stamp}.rds"))
  )
}

# validity date read off the filename, so no analysis script touches SharePoint
clean_file_date <- function(path) {
  as.Date(stringr::str_extract(path, "\\d{8}(?=\\.rds$)"), format = "%Y%m%d")
}

# read-back counterpart to export_clean(): newest export of one dataset, picked
# on the stamp rather than a string sort. "BUT-EVD_BUTEMBO_..." sorts below a
# legacy "BUTEMBO_..." name, so max() on the path returns the older file
latest_clean <- function(name, dir = butembo_project_clean_data_path) {
  paths <- fs::dir_ls(
    dir,
    regexp = glue::glue("{export_prefix}_{name}__\\d{{8}}\\.rds$")
  )

  if (!length(paths)) {
    cli::cli_abort("No {name} export in {dir} - run R/1_prep_data.R first")
  }

  paths[which.max(clean_file_date(paths))]
}

#* LOCAL CACHE ------------------------------

# adm files change rarely; prefer the local copy 1_prep_data.R writes and
# only fall back to SharePoint (sharepoint_dir) when no cache exists yet
read_geo_cached <- function(file, local_dir = local_geobase_dir, sharepoint_dir = sf_data_path) {
  local_path <- fs::path(local_dir, file)
  if (fs::file_exists(local_path)) {
    readRDS(local_path)
  } else {
    readRDS(fs::path(sharepoint_dir, file))
  }
}

# latest_clean() counterpart for the linelist: prefer the local_ll_dir copy
# 1_prep_data.R writes, only falling back to SharePoint if none exists yet
latest_clean_cached <- function(name, local_dir = local_ll_dir) {
  tryCatch(
    latest_clean(name, dir = local_dir),
    error = function(e) latest_clean(name, dir = butembo_project_clean_data_path)
  )
}

#* AGE ------------------------------

# unit is Ans, Mois or Jour, NA meaning Ans. French Excel writes the decimal
# separator as a comma; fixed() because "." as a regex matches everything
age_years <- function(age, age_unit) {
  age <- as.numeric(str_replace(as.character(age), fixed(","), "."))
  floor(case_when(
    age_unit == "Mois" ~ age / 12,
    age_unit == "Jour" ~ age / 365.25,
    .default = age
  ))
}

#* GEO ------------------------------

# harmonise les noms d'aires de santé (adm3) des sitreps sur ceux du fond de carte
clean_adm3 <- function(x) {
  x <- str_to_sentence(str_squish(x))
  case_when(
    str_detect(x, regex("musayi", ignore_case = TRUE)) ~ "Maman Musayi",
    x %in% c("Wanama", "Wanamahi") ~ "Wanamahika",
    x == "Monde" ~ "Mondo",
    x %in% c("Kangike", "Yangike") ~ "Kyangike",
    # Misebere : nouvelle aire de santé rattachée à Kambuli
    x == "Misebere" ~ "Kambuli",
    .default = x
  )
}

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

# Save a gt table to tables_dir as a high-resolution PNG.
# gtsave crops tight to the table, so width follows the table's own content;
# zoom is the resolution multiplier (higher = crisper, larger file).
# font_size sets a consistent (small) text size across all tables.
#
# gtsave bakes `zoom`x pixels into the PNG but tags it at 72 dpi, so Word /
# Quarto treat every table as `zoom` times its true physical size — hence the
# need for hand-tuned out-width per table and the inconsistent apparent scale.
# We rewrite the real resolution (72 * zoom) so the physical size is correct and
# all tables render at one consistent text scale without per-chunk out-width.
save_gt <- function(gt_tbl, file, zoom = 3, font_size = 11, dir = tables_dir) {
  path <- fs::path(dir, file)
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

# Save an htmlwidget or htmltools tag (e.g. a reactable or a div wrapping
# several) to tables_dir as a PNG via headless Chrome. selector crops tight to
# target; zoom / dpi rewrite mirror save_gt() so tables render at one consistent
# physical scale in Word. save_html() keeps the lib/ sidecar in a temp dir,
# avoiding the pandoc dependency of a self-contained file.
save_widget <- function(
  widget,
  file,
  selector = ".reactable",
  zoom = 3,
  dir = tables_dir
) {
  path <- fs::path(dir, file)
  html <- fs::path(fs::file_temp(), "widget.html")
  fs::dir_create(fs::path_dir(html))
  htmltools::save_html(widget, html)
  webshot2::webshot(html, path, selector = selector, zoom = zoom, delay = 0.5)
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


#' Convert Excel Garbage into an Actual Date also turn positx to date
#'
#' @description Convert excel date codes into human (and R) readable dates. Also convert POSITx
#'
#' @param date `str/num` Date code to be converted
harmonize_dates <- function(date) {
  # a Date through the numeric branch is read as an 1899 serial, a century out
  if (methods::is(date, "Date")) {
    date
  } else if (methods::is(date, "POSIXt")) {
    as.Date(date)
  } else if (is.numeric(date)) {
    as.Date(date, origin = "1899-12-30")
  } else {
    # an Excel serial stored as text, otherwise a written-out date
    serial <- suppressWarnings(as.numeric(date))
    dplyr::if_else(
      is.na(serial),
      lubridate::as_date(lubridate::parse_date_time(
        date,
        orders = c("ymd", "dmy"),
        quiet = TRUE
      )),
      as.Date(serial, origin = "1899-12-30")
    )
  }
}
