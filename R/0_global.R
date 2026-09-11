#* BUTEMBO PROJECT org ------------------------------------
library(sf)
library(here)
library(tmap)
library(janitor)
library(patchwork)
library(tidyverse)

source(here::here("R", "theme.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "map_theme.R"))

#* CONFIG ------------------------------------------------

CONFIG <- list(
  # sitreps before this date are incomplete
  sitrep_start = as.Date("2026-06-21"),

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

onedrive <- Sys.getenv("SHAREPOINT_PATH")

# Epicentre compilation
epicentre_clean_cod_data_path <- fs::path(
  onedrive,
  "Ebola Outbreaks - COD_UGA-2026",
  "COD",
  "data"
)

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

#* ETC data --------------------------
etc_ll_path <- fs::dir_ls(
  fs::path(
    butembo_project_data_path,
    "linelists",
    "cte_kitatumba",
    "exports"
  )
) |>
  max()

butembo_project_sf_data_path <- fs::path(
  butembo_project_data_path,
  "spatiale"
)

#sitrep summary
sitrep_path <- fs::path(
  butembo_project_data_path,
  "brute",
  "sitrep",
  "sitrep_summary.xlsx"
)

# the narrative linelist RAW, same file EVD-COD17-incubation-si reads
narr_ll_dir <- fs::path(
  butembo_project_data_path,
  "linelists",
  "ensemble",
  "narrative LL"
)

latest_narr_ll <- fs::path(narr_ll_dir, "ensemble_LL.xlsx")

# second copy of the clean linelist, kept next to the raw export for the team
narr_ll_clean_dir <- fs::path(narr_ll_dir, "clean")

# clean data folder (timestamped exports written by 1_prep_data.R)
butembo_project_clean_data_path <- fs::path(
  butembo_project_data_path,
  "propre"
)

# de-identified exports for colleagues, written by prep_for_sharing.R
butembo_share_data_path <- fs::path(
  butembo_project_data_path,
  "partage"
)

# local, gitignored cache: lets scripts (e.g. the dashboard) reread the adm
# files and cleaned linelist without a live SharePoint connection
local_dir <- here::here("local")
local_geobase_dir <- fs::path(local_dir, "geobase")
local_ll_dir <- fs::path(local_dir, "linelist")
local_hf_dir <- fs::path(local_dir, "hf-visits")

fs::dir_create(c(local_geobase_dir, local_ll_dir, local_hf_dir))

# newest timestamped export of each dataset, from utils.R; linelist prefers
# the local cache first (see latest_clean_cached())
latest_narr_ll_clean <- latest_clean_cached("linelist")
latest_hf_visits_clean <- latest_clean("hf-visits")
#latest_alert_clean <- latest_clean("alert-data")
#latest_contact_clean <- latest_clean("contact-data")

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
hf <- readRDS(fs::path(sf_data_path, "COD_HF_sub_gis.rds"))
