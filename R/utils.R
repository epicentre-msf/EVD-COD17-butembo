#* EXPORTING ------------------------------

# stamp is passed in, so every dataset of one run carries the same one.
# dir is created first: on synced storage it may not be there yet
export_clean <- function(
  x,
  export_prefix,
  name,
  stamp,
  dir = butembo_project_clean_data_path
) {
  fs::dir_create(dir)
  saveRDS(
    x,
    fs::path(dir, glue::glue("{export_prefix}_{name}__{stamp}.rds"))
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

time_stamp <- function() {
  format(Sys.time(), "%Y%m%d")
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
    text <- clean_date_text(date)
    # month names become numbers, which are day-first and must not be read ymd
    written <- !is.na(text) & grepl("^[0-9]{1,2} [0-9]{2} [0-9]{2,4}$", text)

    parsed <- dplyr::if_else(
      written,
      lubridate::as_date(lubridate::dmy(text, quiet = TRUE)),
      lubridate::as_date(lubridate::parse_date_time(
        text,
        orders = c("ymd", "dmy"),
        quiet = TRUE
      ))
    )

    failed <- is.na(serial) & is.na(parsed) & !is.na(text) & text != ""
    if (any(failed)) {
      message(
        "harmonize_dates: ", sum(failed), " values unparsed, set to NA: ",
        paste(utils::head(unique(date[failed]), 20), collapse = " | ")
      )
    }

    dplyr::if_else(
      is.na(serial),
      parsed,
      as.Date(serial, origin = "1899-12-30")
    )
  }
}

# Repairs typed dates ("0CTOBRE", "15Septembre26", "12 juillet 2026") into
# "d mm yyyy" text so month names never depend on the session locale.
clean_date_text <- function(date) {
  x <- as.character(date) |>
    stringi::stri_trans_general("Latin-ASCII") |>
    tolower() |>
    stringr::str_squish()

  # a zero is a typo for "o" only beside a letter and away from other digits
  x <- gsub("(?<=[a-z])0(?![0-9])|(?<![0-9])0(?=[a-z])", "o", x, perl = TRUE)
  x <- gsub("([0-9])([a-z])", "\\1 \\2", x)
  x <- gsub("([a-z])([0-9])", "\\1 \\2", x)

  months <- c(
    "jan" = "01", "fev" = "02", "feb" = "02", "mar" = "03", "avr" = "04",
    "apr" = "04", "mai" = "05", "may" = "05", "juin" = "06", "juil" = "07",
    "aou" = "08", "aug" = "08", "sep" = "09", "oct" = "10", "nov" = "11",
    "dec" = "12"
  )
  for (i in seq_along(months)) {
    x <- gsub(paste0("\\b", names(months)[i], "[a-z]*"), months[[i]], x)
  }

  stringr::str_squish(x)
}

#* Dashboard delays ---------------------------------
# One integer column per ordered pair of dates, named "<earlier>__<later>", as
# the dashboard's delay module reads them.
add_delay_pairs <- function(df, date_vars) {
  pairs <- combn(date_vars, 2, simplify = FALSE)
  for (p in pairs) {
    df[[paste0(p[[1]], "__", p[[2]])]] <- as.integer(
      as.Date(df[[p[[2]]]]) - as.Date(df[[p[[1]]]])
    )
  }
  df
}

#* GEO LAYERS ------------------------------

# Simplify admin polygons with rmapshaper (as in the evd-2026-app): shared
# borders are simplified once, so neighbouring units keep matching edges
# (st_simplify works polygon by polygon, and on lon/lat data with s2 its
# tolerance is in metres). `keep` = share of vertices kept; keep_shapes keeps
# every unit, however small. keep >= 1 returns x unchanged.
simplify_geo <- function(x, keep = 0.1) {
  if (keep >= 1) {
    return(x)
  }
  rmapshaper::ms_simplify(x, keep = keep, keep_shapes = TRUE)
}

# the app draws on a web basemap, so layers need lon/lat
prep_app_layer <- function(x, keep = GEO_SIMPLIFY_KEEP) {
  x |>
    sf::st_transform(4326) |>
    simplify_geo(keep = keep)
}

# Restrict the full geobase to the admin units the linelist references:
# provinces and health zones with cases, and every health area inside those
# zones (the case dots and tooltips show which areas have cases).
# Facilities (`hf`, optional): all those in the kept health zones, plus any
# facility in `hf_pcodes` (e.g. visited facilities) located elsewhere.
# With `save = TRUE` the subsets are written next to the full layers, in
# <geobase_dir>/sf_sub/<country>_{adm1,adm2,adm3,hf}_sub.rds, in their original
# CRS and unsimplified, so maps can reuse them; overwritten on each run.
restrict_geobase <- function(
  ll,
  adm1,
  adm2,
  adm3,
  hf = NULL,
  hf_pcodes = NULL,
  geobase_dir = geobase_version,
  country = "COD",
  save = TRUE
) {
  pcodes_used <- function(cols) {
    cols <- intersect(cols, names(ll))
    unique(na.omit(unlist(ll[cols], use.names = FALSE)))
  }
  adm1_used <- pcodes_used(paste0("adm1_pcode__", c("onset", "notif", "res")))
  adm2_used <- pcodes_used(
    paste0("adm2_pcode__", c("onset", "notif", "res", "comptabilisation"))
  )

  sub <- list(
    adm1 = dplyr::filter(adm1, adm1_pcode %in% adm1_used),
    adm2 = dplyr::filter(adm2, adm2_pcode %in% adm2_used),
    adm3 = dplyr::filter(adm3, adm2_pcode %in% adm2_used)
  )
  if (!is.null(hf)) {
    sub$hf <- hf |>
      dplyr::filter(adm2_pcode %in% adm2_used | pcode %in% hf_pcodes) |>
      # the geobase stores facilities as single-point MULTIPOINTs
      sf::st_cast("POINT", warn = FALSE)
  }

  if (save) {
    sub_dir <- fs::path(geobase_dir, "sf_sub")
    fs::dir_create(sub_dir)
    purrr::iwalk(
      sub,
      \(x, lvl) saveRDS(x, fs::path(sub_dir, glue::glue("{country}_{lvl}_sub.rds")))
    )
  }

  cli::cli_inform(c(
    "i" = "Geobase restricted to the linelist: {nrow(sub$adm1)} province(s), \\
    {nrow(sub$adm2)} health zone(s), {nrow(sub$adm3)} health area(s)\\
    {if (!is.null(sub$hf)) paste0(', ', nrow(sub$hf), ' facilities') else ''}.",
    if (save) c(" " = "Saved to {.path {fs::path(geobase_dir, 'sf_sub')}}")
  ))

  sub
}

#* VACCINATED CASE MATCHING ------------------------------

# normalised so accents and case do not split the same patient
norm_key <- \(x) {
  x |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_squish() |>
    str_to_upper()
}

# id_msf when present, else the patient name (pre-ETC cases have no id_msf)
add_key <- \(d) {
  d |>
    mutate(
      match_key = if_else(
        !is.na(id_msf),
        norm_key(as.character(id_msf)),
        norm_key(nom)
      )
    )
}

to_long <- \(d, version, vars) {
  d |>
    select(all_of(c("match_key", vars))) |>
    # blank cells and "NA" text from excel must not differ from a true NA
    mutate(across(
      all_of(vars),
      \(x) na_if(str_squish(as.character(x)), "") |> na_if("NA")
    )) |>
    pivot_longer(
      all_of(vars),
      names_to = "variable",
      values_to = "value"
    ) |>
    mutate(version = version)
}
