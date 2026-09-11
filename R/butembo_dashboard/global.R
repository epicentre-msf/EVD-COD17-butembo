# Epishiny module for North-Kivu

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(epishiny))

# then set your options. the options below are the defaults
options(
  epishiny.na.label = "(Missing)", # label to be used for NA values in outputs
  epishiny.count.label = "Cases", # if data is un-aggregated, the label to represent row counts
  epishiny.week.letter = "W", # letter to represent 'Week'. Change to S for 'Semaine' etc
  epishiny.week.start = 1 # day the epiweek starts on. 1 = Monday, 7 = Sunday
)

#* PATH ------------------------------------------------
source(here::here("R", "0_global.R"))

#* Admin ------------------------------------------------
# adm1/adm2/adm3 are already loaded by 0_global.R (local cache if present,
# SharePoint otherwise) - just filter down to the North-Kivu / Butembo scope
adm1_nk <- adm1 |>
  filter(adm1_name %in% c("Nord-Kivu"))
adm2_nk <- adm2 |>
  filter(adm1_name %in% c("Nord-Kivu"))
adm3_nk <- adm3 |>
  filter(adm2_name %in% c("Butembo", "Katwa"))

#* Import DATA  ------------------------------------------------
# app_data.rds is built by 1_prep_data.R: already cleaned, health-zone
# filtered, and cached locally so the dashboard never has to touch SharePoint
app_data <- readRDS(fs::path(local_dir, "app_data.rds"))
but_ll <- app_data$linelist

# epishiny assigns `group_pal` positionally to the grouping factor's levels, so
# order EVD_status and build the palette from the classes actually present.
evd_status_cols <- c(
  "Confirmed" = "#9e2a2b", # dark red
  "Probable" = "#e09f3e", # amber
  "Suspect" = "#bdbdbd", # grey
  "Non cas" = "#6d85b6" # muted blue
)

evd_present <- intersect(names(evd_status_cols), unique(but_ll$EVD_status))
but_ll <- but_ll |>
  mutate(EVD_status = factor(EVD_status, levels = evd_present))
evd_pal <- unname(evd_status_cols[evd_present])

but_ll_conf <- but_ll |> filter(EVD_status %in% c("Confirmed"))

#* Geo data ------------------------------------------------

but_ll <- but_ll |>
  left_join(
    select(adm1_nk, adm1_name, adm1_pcode__onset = adm1_pcode),
    join_by(adm1_name__onset == adm1_name)
  ) |>
  left_join(
    select(adm2_nk, adm2_name, adm2_pcode__onset = adm2_pcode),
    join_by(adm2_name__onset == adm2_name)
  ) |>
  left_join(
    select(adm3_nk, adm3_name, adm3_pcode__onset = adm3_pcode),
    join_by(adm3_name__onset == adm3_name)
  )

geo_data <- list(
  geo_layer(
    layer_name = "Province", # name of the boundary layer
    sf = adm1_nk, # sf object with boundary polygons
    name_var = "adm1_name", # column with place names
    pop_var = "adm1_pop", # column with population data (optional)
    join_by = c("pcode" = "adm1_pcode__onset") # geo to data join vars: LHS = sf, RHS = data
  ),
  geo_layer(
    layer_name = "Health Zone",
    sf = adm2_nk,
    name_var = "adm2_name",
    pop_var = "adm2_pop",
    join_by = c("pcode" = "adm2_pcode__onset")
  ),
  geo_layer(
    layer_name = "Health Area",
    sf = adm3_nk,
    name_var = "adm3_name",
    join_by = c("pcode" = "adm3_pcode__onset")
  )
)

# define date variables in data as named list to be used in app
date_vars <- c(
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

# value boxes + their helpers. sourced explicitly: shiny only auto-loads an
# R/ subdirectory for app.R apps, not the ui.R / server.R triad
source(here::here("R", "butembo_dashboard", "mod_vb.R"))

# serve www/ (logos, stylesheet) to the browser
addResourcePath("assets", here::here("R", "butembo_dashboard", "www"))
