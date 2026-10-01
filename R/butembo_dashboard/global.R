suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(epishiny))
suppressPackageStartupMessages(library(mapgl))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(reactable))

options(
  epishiny.na.label = "(Missing)",
  epishiny.count.label = "Cases",
  epishiny.week.letter = "W",
  epishiny.week.start = 1
)

#* Import DATA  ------------------------------------------------
app_data_path <- fs::path("data", "app_data.rds")

# App data
app_data <- readRDS(app_data_path)

# Linelist data
but_ll <- app_data$linelist

# one row per recorded health-facility visit, and the FOSA layer to map them
hf_visits <- app_data$hf_visits

# one row per sample tested, for the Lab data tab
lab_data <- app_data$lab_data
hf_geo <- app_data$hf_geo
# cases per located site, and flows between sites; not yet mapped in the app
hf_cases <- app_data$hf_cases
hf_flows <- app_data$hf_flows

# completeness and geo-match tables, built in prep for the Data quality tab
quality <- app_data$quality

#* Geo data ------------------------------------------------

geo_data <- list(
  geo_layer(
    layer_name = "Province", # name of the boundary layer
    sf = app_data$admin_data$adm1,
    name_var = "adm1_name", # column with place names
    pop_var = "adm1_pop", # column with population data (optional)
    join_by = c("pcode" = "adm1_pcode__notif") # geo to data join vars: LHS = sf, RHS = data
  ),
  geo_layer(
    layer_name = "Health Zone",
    sf = app_data$admin_data$adm2,
    name_var = "adm2_name",
    pop_var = "adm2_pop",
    join_by = c("pcode" = "adm2_pcode__notif")
  ),
  geo_layer(
    layer_name = "Health Area",
    sf = app_data$admin_data$adm3,
    name_var = "adm3_name",
    join_by = c("pcode" = "adm3_pcode__notif")
  )
)

# same three levels, joined on residence instead of notification pcodes
geo_data_res <- list(
  geo_layer(
    layer_name = "Province",
    sf = app_data$admin_data$adm1,
    name_var = "adm1_name",
    pop_var = "adm1_pop",
    join_by = c("pcode" = "adm1_pcode__res")
  ),
  geo_layer(
    layer_name = "Health Zone",
    sf = app_data$admin_data$adm2,
    name_var = "adm2_name",
    pop_var = "adm2_pop",
    join_by = c("pcode" = "adm2_pcode__res")
  ),
  geo_layer(
    layer_name = "Health Area",
    sf = app_data$admin_data$adm3,
    name_var = "adm3_name",
    join_by = c("pcode" = "adm3_pcode__res")
  )
)

# facility markers shown on the map
facilities <- tibble::tribble(
  ~site           , ~lon     , ~lat       ,
  "CTE Kitatumba" , 29.28584 , 0.1456292  ,
  "CT UCG"        , 29.26221 , 0.1236155  ,
  "CT Matanda"    , 29.29477 , 0.1235159  ,
  "CTE Katwa"     , 29.30645 , 0.09298772
)

# define date variables in data as named list to be used in app
date_vars <- c(
  "Date of Lab confirmation" = "date_lab_result_1",
  "Date of notification" = "date_notification",
  "Date of onset" = "date_symptom_onset",
  "Date of admission" = "date_admission_eff",
  "Date of exit" = "date_exit_eff"
)

# define categorical grouping variables
# in data as named list to be used in app
group_vars <- c(
  "Health zone" = "adm2_comptabilisation",
  "Health structure" = "isolation_site_id",
  "Sex" = "sex",
  "Outcome" = "type_of_exit"
)

lab_date_vars <-c("Date of lab result" = "date_lab_result")

lab_group_vars <- c(
  "Result" = "lab_result",
  "Source" = "source",
  "Sample type" = "sample_type"
)

# ! Modules ----------------------------

# value boxes module
source("mod_vb.R")

# lab tab: value boxes + samples curve, reuses the vb_* helpers above
source("mod_lab.R")

# place (map) module
source("mod_map_place.R")

# data quality module
source("mod_quality.R")

# delays module
source("mod_delays.R")

# epicurves by notification health zone
source("mod_epicurve_hz.R")

# health facilities visited before isolation
source("mod_facilities.R")

# chronological order, as the delay module subtracts earlier from later
delay_date_vars <- c(
  "Date of onset" = "date_symptom_onset",
  "Date of notification" = "date_notification",
  "Date of admission" = "date_admission_eff",
  "Date of Lab confirmation" = "date_lab_result_1",
  "Date of exit" = "date_exit_eff"
)

# serve www/ (logos, stylesheet) to the browser
addResourcePath("assets", "www")

#* Color Palettes  -----------------------

# epishiny assigns `group_pal` positionally to the grouping factor's levels, so
# order EVD_status and build the palette from the classes actually present.
evd_status_cols <- c(
  "Confirmed" = "#9e2a2b", # dark red
  "Probable" = "#e09f3e", # amber
  "Suspect" = "#bdbdbd", # grey
  "Non cas" = "#6d85b6" # muted blue
)

evd_pal <- unname(evd_status_cols[c("Confirmed", "Probable")])

# positional on the lab_result levels: Négatif pale grey, Positif dark red
lab_pal <- c("#d9d9d9", "#8b0000")

# https://apps.epicentre-msf.org/testing/
# docker run --rm -p 5858:3838 \
#     -v /home/epicentre/EVD-COD17-butembo/R/butembo_dashboard:/root/app \
#     bvd-app \
#     R -e "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"
