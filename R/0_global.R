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

onedrive <- Sys.getenv("SHAREPOINT_PATH")

# Epicentre compilation
epicentre_clean_cod_data_path <- fs::path(
  onedrive,
  "Ebola Outbreaks - COD_UGA-2026",
  "COD",
  "data"
)

# latest epicentre compilation path
compilation_linelist_path <- fs::dir_ls(
  fs::path(epicentre_clean_cod_data_path, "linelist"),
  regexp = ".rds"
) |>
  max()

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
    "Liste-lin\u00e9aire CTE Kitatumba",
    "exports"
  )
) |>
  max()

butembo_project_sf_data_path <- fs::path(
  butembo_project_data_path,
  "spatiale"
)

# raw linelist data
butembo_project_raw_data_path <- fs::path(
  butembo_project_data_path,
  "brute",
  "LL"
)

#sitrep summary
sitrep_path <- fs::path(
  butembo_project_data_path,
  "brute",
  "sitrep",
  "sitrep_summary.xlsx"
)

# the latest narrative linelist export RAW
latest_narr_ll <- fs::dir_ls(fs::path(
  butembo_project_data_path,
  "LL-epic",
  "narrative LL",
  "export_ll"
)) |>
  max()

# clean data folder (timestamped exports written by 1_prep_data.R)
butembo_project_clean_data_path <- fs::path(
  butembo_project_data_path,
  "propre"
)

#latest narrative linelist clean
latest_narr_ll_clean <- fs::dir_ls(
  butembo_project_clean_data_path,
  regexp = "BUTEMBO_linelist"
) |>
  max()

# the transmission data
transmission_path <- fs::path(
  butembo_project_data_path,
  "LL-epic",
  "narrative LL",
  "butembo_transmission_pairs.xlsx"
)

# latest transmission data clean
latest_transmission_clean <- fs::dir_ls(
  butembo_project_clean_data_path,
  regexp = "BUTEMBO_transmission-data"
) |>
  max()

# latest alerts data clean
latest_alert_clean <- fs::dir_ls(
  butembo_project_clean_data_path,
  regexp = "BUTEMBO_alert-data"
) |>
  max()

# latest contacts data clean
latest_contact_clean <- fs::dir_ls(
  butembo_project_clean_data_path,
  regexp = "BUTEMBO_contact-data"
) |>
  max()

#output directory
out_dir <- fs::path("output")

#* Spatial data

sf_data_path <- fs::path(butembo_project_sf_data_path, "rds")

#adm1 <- readRDS(fs::path(sf_data_path, "COD_adm1_sub.rds"))
adm2 <- readRDS(fs::path(sf_data_path, "COD_adm2_sub.rds"))
adm3 <- readRDS(fs::path(sf_data_path, "COD_adm3_sub.rds"))
hf <- readRDS(fs::path(sf_data_path, "COD_HF_sub_gis.rds"))
