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
  week_start = 1
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

hf_flow_gis_path <- fs::path(
  butembo_project_data_path,
  "spatiale",
  "etablissements_sante",
  "Nombre de cas par FOSA",
  "cod_butembo_itinéraires_cas_confirmés.json"
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


#* Spatial data --------------------
butembo_project_sf_data_path <- fs::path(
  butembo_project_data_path,
  "spatiale"
)

#* LABORATORY DATA ------------------
lab_dir <- fs::path(butembo_project_data_path, "brute", "labo")

latest_inrb_lab_path <- fs::dir_ls(
  fs::path(lab_dir),
  regex = 'Butembo_MVE17_Partage_Resultats'
) |>
  max()

latest_mobile_lab <- fs::dir_ls(
  lab_dir,
  regex = "MVE_17_LABO_MOBILE_BUTEMBO"
) |>
  max()

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

# LL facility names matched to the master geobase, with a geometry
hf_geo_csv <- fs::path(
  butembo_project_sf_data_path,
  "etablissements_sante",
  "cod_butembo_matching_LL_GIS_MDB.csv"
)

local_dir <- here::here("local")
local_geobase_dir <- fs::path(local_dir, "geobase")
local_ll_dir <- fs::path(local_dir, "linelist")
local_hf_dir <- fs::path(local_dir, "hf-visits")
local_hf_geo_csv <- fs::path(local_hf_dir, fs::path_file(hf_geo_csv))

fs::dir_create(c(local_geobase_dir, local_ll_dir, local_hf_dir))

#* OUTPUT DIRECTORIES ------------------------------------
out_dir <- here::here("output")
plots_dir <- fs::path(out_dir, "plots") # ggsave / tmap_save targets
tables_dir <- fs::path(out_dir, "tables") # gt and reactable pngs, xlsx exports
rds_dir <- fs::path(out_dir, "rds") # intermediates the report quotes

fs::dir_create(c(plots_dir, tables_dir, rds_dir))

#* Spatial data

sf_data_path <- fs::path(butembo_project_sf_data_path, "rds")

adm1 <- read_geo_cached("COD_adm1_sub.rds")
adm2 <- read_geo_cached("COD_adm2_sub.rds")
adm3 <- read_geo_cached("COD_adm3_sub.rds")
