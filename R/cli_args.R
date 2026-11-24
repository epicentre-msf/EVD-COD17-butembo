# Terminal flags for 1_prep_data.R, sourced into its CONFIG block.
# Left-hand value is the local default; flags override it.
# --no-export  --no-send  --lab  --update-geobase  --geo-keep=<0-1>

cli_args <- commandArgs(trailingOnly = TRUE)
geo_keep_arg <- sub(
  "^--geo-keep=",
  "",
  grep("^--geo-keep=", cli_args, value = TRUE)
)

EXPORT_TO_SHAREPOINT <- TRUE && !"--no-export" %in% cli_args
SEND_TO_SERVER <- TRUE && !"--no-send" %in% cli_args
# FALSE reuses lab_data from the last app_data.rds, for when the lab exports are not to hand
PREP_LAB_DATA <- FALSE || "--lab" %in% cli_args
# TRUE copies the latest OutbreakTools geobase into spatiale/geobase before loading it
UPDATE_GEOBASE <- FALSE || "--update-geobase" %in% cli_args
# share of polygon vertices kept by rmapshaper::ms_simplify (simplify_geo() in
# utils.R); the app layers were slow at full detail. 1 keeps them as-is
GEO_SIMPLIFY_KEEP <- if (length(geo_keep_arg)) as.numeric(geo_keep_arg) else 0.1
