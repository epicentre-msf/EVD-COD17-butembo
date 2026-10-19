suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(bslib))
suppressPackageStartupMessages(library(epishiny))
suppressPackageStartupMessages(library(mapgl))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(reactable))

# French month and day names for every highcharter chart
options(
  highcharter.lang = list(
    months = c(
      "Janvier",
      "Février",
      "Mars",
      "Avril",
      "Mai",
      "Juin",
      "Juillet",
      "Août",
      "Septembre",
      "Octobre",
      "Novembre",
      "Décembre"
    ),
    shortMonths = c(
      "janv.",
      "févr.",
      "mars",
      "avr.",
      "mai",
      "juin",
      "juil.",
      "août",
      "sept.",
      "oct.",
      "nov.",
      "déc."
    ),
    weekdays = c(
      "Dimanche",
      "Lundi",
      "Mardi",
      "Mercredi",
      "Jeudi",
      "Vendredi",
      "Samedi"
    ),
    shortWeekdays = c("Dim", "Lun", "Mar", "Mer", "Jeu", "Ven", "Sam")
  )
)

options(
  epishiny.na.label = "(Manquant)",
  epishiny.count.label = "Cas",
  epishiny.week.letter = "S",
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
# cases per located site, and flows between sites
hf_cases <- app_data$hf_cases
hf_flows <- app_data$hf_flows

# add_flowmap() wants id/lat/lon nodes and origin/dest/count edges
flow_locations <- hf_cases |>
  dplyr::filter(!is.na(lon), !is.na(lat)) |>
  dplyr::transmute(id = raw_name, name = hf_name, lon, lat)
stopifnot(!anyDuplicated(flow_locations$id))

flow_edges <- hf_flows |>
  dplyr::transmute(origin = from, dest = to, count = n_cases) |>
  dplyr::filter(origin %in% flow_locations$id, dest %in% flow_locations$id)
message(
  "flowmap: kept ",
  nrow(flow_edges),
  " of ",
  nrow(hf_flows),
  " flows (an end without coordinates)"
)

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
    layer_name = "Zone de santé",
    sf = app_data$admin_data$adm2,
    name_var = "adm2_name",
    pop_var = "adm2_pop",
    join_by = c("pcode" = "adm2_pcode__notif")
  ),
  geo_layer(
    layer_name = "Aire de santé",
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
    layer_name = "Zone de santé",
    sf = app_data$admin_data$adm2,
    name_var = "adm2_name",
    pop_var = "adm2_pop",
    join_by = c("pcode" = "adm2_pcode__res")
  ),
  geo_layer(
    layer_name = "Aire de santé",
    sf = app_data$admin_data$adm3,
    name_var = "adm3_name",
    join_by = c("pcode" = "adm3_pcode__res")
  )
)

# opening view of both maps: the Katwa and Butembo health zones, lightly padded
focus_zones <- app_data$admin_data$adm2[
  app_data$admin_data$adm2$adm2_name %in% c("Katwa", "Butembo"),
]
# the zone filter returning nothing would leave an empty bbox, so fall back to all zones
focus_bbox <- sf::st_bbox(
  if (nrow(focus_zones) > 0) focus_zones else app_data$admin_data$adm2
)
focus_pad <- c(
  (focus_bbox[["xmax"]] - focus_bbox[["xmin"]]) * 0.05,
  (focus_bbox[["ymax"]] - focus_bbox[["ymin"]]) * 0.05
)
focus_bbox[c("xmin", "xmax")] <- focus_bbox[c("xmin", "xmax")] +
  c(-focus_pad[1], focus_pad[1])
focus_bbox[c("ymin", "ymax")] <- focus_bbox[c("ymin", "ymax")] +
  c(-focus_pad[2], focus_pad[2])

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
  "Date de confirmation labo" = "date_lab_result_1",
  "Date de notification" = "date_notification",
  "Date de début des symptômes" = "date_symptom_onset",
  "Date d'admission" = "date_admission_eff",
  "Date de sortie" = "date_exit_eff"
)

# define categorical grouping variables
# in data as named list to be used in app
group_vars <- c(
  "Zone de santé" = "adm2_comptabilisation",
  "Groupe d'âge" = "age_group",
  "Sexe" = "sex",
  "Isolé" = "isolated",
  "Décès hors isolement" = "dead_out_isolation",
  "Type de transmission" = "transmission_type_1",
  "Décédé à la notification" = "dead_upon_notif_grp",
  "Issue" = "type_of_exit",
  "Infection locale" = "infection_butembo"
)

# an "Unknown" level would take a positional epicurve colour; as NA, epishiny
# labels it (Manquant) and paints it na_col. "Not applicable" stays a level.
unknown_to_na <- \(x) {
  x[x %in% "Unknown"] <- NA
  if (is.factor(x)) factor(x, levels = setdiff(levels(x), "Unknown")) else x
}
n_unknown <- vapply(
  but_ll[unname(group_vars)],
  \(x) sum(x %in% "Unknown"),
  integer(1)
)
message(
  "Unknown set to NA: ",
  paste0(
    names(n_unknown)[n_unknown > 0],
    " (",
    n_unknown[n_unknown > 0],
    ")",
    collapse = ", "
  )
)
but_ll <- but_ll |>
  dplyr::mutate(dplyr::across(
    dplyr::all_of(unname(group_vars)) &
      tidyselect::where(\(x) "Unknown" %in% x),
    unknown_to_na
  ))

lab_date_vars <- c("Date du résultat labo" = "date_lab_result")

lab_group_vars <- c(
  "Résultat" = "lab_result",
  "Source" = "source",
  "Type d'échantillon" = "sample_type",
  "Premier prélèvement ?" = "first_sample"
)

# labels shared by every time_ui() (surveillance and lab epicurves)
time_ui_labs <- list(
  opts_btn_lab = "Options du graphique",
  date_lab = "Axe des dates",
  date_int_lab = "Intervalle de dates",
  date_intervals = c("Jour" = "day", "Semaine" = "week", "Mois" = "month"),
  count_vars_lab = "Indicateur",
  groups_lab = "Grouper les données par",
  no_grouping_lab = "Aucun groupe",
  bar_stacking_lab = "Empilement des barres",
  cumul_data_lab = "Afficher les données cumulées ?",
  zoom_control_lab = "Ajouter un curseur de zoom ?"
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

# confirmed cases by health zone
source("mod_zs_table.R")

# chronological order, as the delay module subtracts earlier from later
delay_date_vars <- c(
  "Date de début des symptômes" = "date_symptom_onset",
  "Date de notification" = "date_notification",
  "Date d'admission" = "date_admission_eff",
  "Date de confirmation labo" = "date_lab_result_1",
  "Date de sortie" = "date_exit_eff"
)

# serve www/ (logos, stylesheet) to the browser
addResourcePath("assets", "www")

#* Color Palettes  -----------------------

# one light grey for every "no value" level, so missing never reads as a category
na_col <- "#bdbdbd"
na_labels <- c("Unknown", "Not applicable")

# epishiny assigns `group_pal` positionally to the grouping factor's levels, so
# order EVD_status and build the palette from the classes actually present.
evd_status_cols <- c(
  "Confirmed" = "#9e2a2b", # dark red
  "Probable" = "#e09f3e", # amber
  "Suspect" = "#f2d16b", # pale yellow
  "Non cas" = "#6d85b6" # muted blue
)

evd_pal <- unname(evd_status_cols[c("Confirmed", "Probable")])

# one palette per grouping variable, named by level so a level keeps its
# colour whatever subset of levels the filters leave

group_pals <- list(
  EVD_status = evd_status_cols,
  adm2_comptabilisation = c(
    "Katwa" = "#1b6ca8",
    "Butembo" = "#e08a1e",
    "Musienene" = "#3a9d8f",
    "Kalunguta" = "#8e5ea2",
    "Vuhovi" = "#c9a227",
    "Masereka" = "#6baed6",
    "Kyondo" = "#b5651d",
    "Lubero" = "#4d4d8f",
    "Biena" = "#8fbf6a",
    "Manguredjipa" = "#d98cb3"
  ),
  sex = c("Male" = "#2a6f97", "Female" = "#e9a03b"),
  age_group = c(
    "<1" = "#d9f0a3",
    "1-4" = "#addd8e",
    "5-9" = "#78c679",
    "10-19" = "#41ab5d",
    "20-29" = "#238443",
    "30-39" = "#006837",
    "40-49" = "#004529",
    "50-59" = "#00331f",
    "60+" = "#002115"
  ),
  isolated = c("Yes" = "#2a6f97", "No" = "#9e2a2b"),
  dead_out_isolation = c(
    "Yes" = "#9e2a2b",
    "No" = "#2a6f97",
    "Not applicable" = na_col
  ),
  dead_upon_notif_grp = c(
    "Dead on notification" = "#9e2a2b",
    "Alive on notification" = "#3a9d8f"
  ),
  transmission_type_1 = c(
    "Familiale" = "#9e2a2b",
    "Funérailles" = "#4d4d8f",
    "Nosocomiale" = "#e09f3e",
    "Sanguin directe" = "#d98cb3",
    "Soins au malade" = "#3a9d8f",
    "Visite occasionnelle" = "#6baed6",
    "Autre" = "#8e5ea2",
    "Unknown" = na_col
  ),
  type_of_exit = c(
    "Recovered" = "#3a9d8f",
    "Abandoned" = "#e09f3e",
    "Died" = "#9e2a2b"
  ),
  infection_butembo = c(
    "Local" = "#9e2a2b",
    "Imported" = "#e09f3e",
    "Unknown" = na_col
  ),
  lab_result = c("Négatif" = "#3a9d8f", "Positif" = "#8b0000"),
  source = c("INRB Béni" = "#2a6f97", "Laboratoire Mobile" = "#e9a03b"),
  first_sample = c(
    "Oui" = "#2a6f97",
    "Non" = "#e09f3e",
    "Unknown" = na_col
  ),
  sample_type = c(
    "Sang total" = "#9e2a2b",
    "Ecouvillon oral" = "#6d85b6",
    "Lait Maternel" = "#e09f3e"
  )
  # isolation_site_id (70 levels) left to epishiny's pal20 fallback
)

# colours for the levels of `var`, named; unmapped levels fall back to pal20
group_colours <- function(
  var,
  levels,
  missing_label = getOption("epishiny.na.label")
) {
  known <- group_pals[[var]]
  fallback <- epishiny:::epi_pals()$pal20
  cols <- known[levels]
  cols[levels %in% c(missing_label, na_labels)] <- na_col
  unmapped <- is.na(cols)
  cols[unmapped] <- rep_len(fallback, sum(unmapped))
  unname(cols)
}

# positional on the lab_result levels: Négatif then Positif
lab_pal <- unname(group_pals$lab_result)

# https://apps.epicentre-msf.org/testing/
# docker run --rm -p 5858:3838 \
#     -v /home/epicentre/EVD-COD17-butembo/R/butembo_dashboard:/root/app \
#     bvd-app \
#     R -e "shiny::runApp('/root/app', port = 3838, host = '0.0.0.0')"
