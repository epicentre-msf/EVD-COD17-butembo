#* BUTEMBO PROJECT org ------------------------------------
suppressPackageStartupMessages({
  library(sf)
  library(here)
  library(tmap)
  library(janitor)
  library(patchwork)
  library(tidyverse)
})

source(here::here("R", "utils.R"))

#* CONFIG ------------------------------------------------

CONFIG <- list(
  export_prefix = "BUT-EVD",

  # under-fives split out: they are a large share of the cases
  age_breaks = c(0, 1, 5, seq(10, 60, 10), Inf),
  age_labs = c(
    "<1",
    "1-4",
    "5-9",
    "10-19",
    "20-29",
    "30-39",
    "40-49",
    "50-59",
    "60+"
  ),

  # ISO weeks, Monday start
  week_start = 1,

  # ETC admissions kept up to and including this ISO year-week (yyyyww)
  etc_cut_isoweek = 202641
)

#* SHAREPOINT PATH ------------------------------
onedrive <- Sys.getenv("SHAREPOINT_PATH")

#* BUTEMBO PATH ------------------------------------

if (Sys.info()[["nodename"]] == "dell-ff") {
  butembo_project_path <- fs::path(
    onedrive,
    'OCP - Workplace RDC - CD153 EBOLA BUTEMBO',
    '10 Médical',
    '17 Epidemiologie'
  )
} else {
  butembo_project_path <- fs::path(
    onedrive,
    'OCP - Workplace RDC - 17 Epidemiologie'
  )
}

# Sharepoint path to the butembo project data
butembo_project_data_path <- fs::path(
  butembo_project_path,
  "Donnees"
)

#*TRASMISSION DATA
transmission_dir <- fs::path(
  butembo_project_data_path,
  "linelists",
  "ensemble",
  "transmission_data"
)

#* ETC data --------------------------
etc_ll_path <- fs::path(butembo_project_data_path, "linelists")

# kitatumba
etc_ll_path_kit <- fs::dir_ls(
  fs::path(
    etc_ll_path,
    "cte_kitatumba",
    "export_nominatif"
  ),
  regexp = "/[^/~][^/]*\\.xlsb$"
) |>
  max()

# UCG
etc_ll_path_ucg <- fs::dir_ls(
  fs::path(
    etc_ll_path,
    "ct_ucg",
    "export_nominatif"
  ),
  regexp = "/[^/~][^/]*\\.xlsb$"
) |>
  max()

# latest compilation from Patrick
etc_compilation_dir <- fs::path(
  fs::path(
    onedrive,
    'Epi Platform-BVD – Data and Tools - Documents'
  ),
  "Data",
  "data",
  "COD"
)

# linelist
latest_etc_compilation <- fs::dir_ls(fs::path(
  etc_compilation_dir,
  "linelist"
)) |>
  max()

# Bed occupancy rate
latest_etc_compilation_bor <- fs::dir_ls(fs::path(
  etc_compilation_dir,
  "linelist-bed-occupancy"
)) |>
  max()

#* ALERT DATA -----------------------
alert_dir <- fs::path(butembo_project_data_path, "brute", "alertes")

#* Spatial data --------------------
butembo_project_sf_data_path <- fs::path(
  butembo_project_data_path,
  "spatiale",
  "geobase"
)

#* LABORATORY DATA ------------------
lab_dir <- fs::path(butembo_project_data_path, "brute", "labo")

latest_inrb_lab_path <- fs::dir_ls(
  fs::path(lab_dir, "PartageResults_INRB"),
  regex = 'Butembo_MVE17_Partage_Resultats'
) |>
  max()

# newest first, so we can fall back to an earlier export
mobile_lab_files <- fs::dir_ls(
  fs::path(lab_dir, "PartageResults_Mobile"),
  regex = "MVE_17_LABO_MOBILE_BUTEMBO"
) |>
  sort(decreasing = TRUE)

# latest exports sometimes drop the result column
mobile_result_col <- "kit_danalyse_altona_filoscreen_1_0_resultats_pos_neg"

has_mobile_result_col <- function(path) {
  mobile_result_col %in%
    names(janitor::clean_names(rio::import(path, n_max = 0)))
}

latest_mobile_lab <- Find(has_mobile_result_col, mobile_lab_files)
stopifnot(
  "no mobile lab export has the result column" = !is.null(latest_mobile_lab)
)

if (latest_mobile_lab != mobile_lab_files[[1]]) {
  cli::cli_alert_warning(
    "{fs::path_file(mobile_lab_files[[1]])} lacks the result column; using {fs::path_file(latest_mobile_lab)}"
  )
}

#* ENSEMBLE LINELIST ----------------
narr_ll_dir <- fs::path(
  butembo_project_data_path,
  "linelists",
  "ensemble",
  "narrative LL"
)

latest_narr_ll <- fs::path(narr_ll_dir, "ensemble_LL.xlsx")

# clean ensemble
narr_ll_clean_dir <- fs::path(narr_ll_dir, "clean")

# de-identified exports for colleagues, written by prep_for_sharing.R
butembo_share_data_path <- fs::path(
  butembo_project_data_path,
  "partage"
)

# matched vaccinated linelists, used by vaccinated_cases.R
butembo_matched_ll_dir <- fs::path(
  onedrive,
  "Ebola Outbreaks - COD_UGA-2026",
  "COD",
  "data-raw",
  "Butembo-surv",
  "data"
)

check_match_dir <- fs::path(butembo_matched_ll_dir, "check-match")
patrick_match_path <- fs::path(
  check_match_dir,
  "vaccinated_check_PB_2026-10-02.xlsx"
)
vax_ll_out_dir <- fs::path(butembo_matched_ll_dir, "vaccinated_linelist")
to_match_dir <- fs::path(butembo_matched_ll_dir, "to-be-matched")
# nominative confirmed cases with positive tests, written by prep_for_sharing.R
lab_data_dir <- fs::path(butembo_matched_ll_dir, "lab-data")

local_dir <- here::here("local")
local_ll_dir <- fs::path(local_dir, "linelist")
local_hf_dir <- fs::path(local_dir, "hf-visits")
local_dupes_dir <- fs::path(local_dir, "dupes")
local_transmission_dir <- fs::path(local_dir, "transmission")
local_lab_dir <- fs::path(local_dir, "lab-data")
local_quality_dir <- fs::path(local_dir, "quality")
local_10th_dir <- fs::path(local_dir, "10th-outbreak")
local_tables_dir <- fs::path(local_dir, "tables")
local_alert_dir <- fs::path(local_dir, "alerts")

fs::dir_create(c(
  local_tables_dir,
  local_alert_dir,
  local_ll_dir,
  local_hf_dir,
  local_dupes_dir,
  local_transmission_dir,
  local_lab_dir,
  local_quality_dir
))

#* OUTPUT DIRECTORIES ------------------------------------
out_dir <- here::here("output")
plots_dir <- fs::path(out_dir, "plots") # ggsave / tmap_save targets
tables_dir <- fs::path(out_dir, "tables") # gt and reactable pngs, xlsx exports
rds_dir <- fs::path(out_dir, "rds") # intermediates the report quotes
kit_flux_dir <- fs::path(tables_dir, "kitatumba_flux") # weekly Kitatumba admissions/exits

kayna_lab_dir <- fs::path(plots_dir, "kayna_lab") # Kayna and INRB Béni lab epicurves

fs::dir_create(c(plots_dir, tables_dir, rds_dir, kit_flux_dir, kayna_lab_dir))

#* Spatial data

# --update-geobase (1_prep_data.R): copy the latest OutbreakTools geobase first,
# so the version picked below is the new one
if (isTRUE(get0("UPDATE_GEOBASE", ifnotfound = FALSE))) {
  source(here::here("R", "update_geobase.R"))
  # $updated is FALSE when that version is already here (nothing copied)
  geobase_update <- update_geobase(
    "COD",
    butembo_geo_path = butembo_project_sf_data_path
  )
}

# latest geobase copied by update_geobase()
geobase_version <- fs::dir_ls(
  butembo_project_sf_data_path,
  type = "directory",
  regexp = "COD__"
) |>
  max()
sf_data_path <- fs::path(geobase_version, "sf")

adm1 <- readRDS(fs::path(sf_data_path, "COD_adm1.rds"))
adm2 <- readRDS(fs::path(sf_data_path, "COD_adm2.rds"))
adm3 <- readRDS(fs::path(sf_data_path, "COD_adm3.rds"))
