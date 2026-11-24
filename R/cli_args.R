# Terminal flags for 1_prep_data.R, sourced into its CONFIG block.
# Left-hand value is the local default; flags override it.
# --no-export  --no-send  --lab  --geo-tol=<degrees>

cli_args <- commandArgs(trailingOnly = TRUE)
geo_tol_arg <- sub(
  "^--geo-tol=",
  "",
  grep("^--geo-tol=", cli_args, value = TRUE)
)

EXPORT_TO_SHAREPOINT <- TRUE && !"--no-export" %in% cli_args
SEND_TO_SERVER <- TRUE && !"--no-send" %in% cli_args
# FALSE reuses lab_data from the last app_data.rds, for when the lab exports are not to hand
PREP_LAB_DATA <- FALSE || "--lab" %in% cli_args
# degrees, about 50 m; the app layers were slow at full detail. 0 keeps them as-is
GEO_SIMPLIFY_TOL <- if (length(geo_tol_arg)) as.numeric(geo_tol_arg) else 0.0005
