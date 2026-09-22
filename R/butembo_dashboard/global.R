suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(epishiny))

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
  "Health zone" = "adm2_name__onset",
  "Health area" = "adm3_name__onset",
  "Health structure" = "isolation_site_id",
  "Sex" = "sex",
  "EVD status" = "EVD_status",
  "Outcome" = "type_of_exit"
)

# ! Modules ----------------------------

# value boxes module
source("mod_vb.R")

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

# https://apps.epicentre-msf.org/testing/
# docker run --rm -p 5858:3838 \
#     -v /home/epicentre/EVD-COD17-butembo/R/butembo_dashboard:/root/app \
#     bvd-app \
#     R -e "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"
