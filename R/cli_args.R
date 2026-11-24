# Terminal flags for 1_prep_data.R, sourced into its CONFIG block.
# Left-hand value is the local default; flags override it.
# --no-export  --no-send  --update-lab  --update-geobase

cli_args <- commandArgs(trailingOnly = TRUE)

EXPORT_TO_SHAREPOINT <- TRUE && !"--no-export" %in% cli_args
SEND_TO_SERVER <- TRUE && !"--no-send" %in% cli_args
# FALSE reuses lab_data from the last app_data.rds, for when the lab exports are not to hand
PREP_LAB_DATA <- FALSE || "--update-lab" %in% cli_args
# TRUE copies the latest OutbreakTools geobase into spatiale/geobase before loading it
UPDATE_GEOBASE <- FALSE || "--update-geobase" %in% cli_args
