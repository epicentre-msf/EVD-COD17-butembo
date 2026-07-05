# Butembo situation report — one-command render pipeline.

setwd(here::here())

# --- 1. Regenerate outputs --------------------------------------------------
# 0_butembo_prep_data.R refreshes the cleaned positives rds from SharePoint;
# the plotting scripts then read it and write PNGs to output/butembo/.
scripts <- c(
  "1_prep_data.R", # must run first: prepares the clean data
  "2_overview.R", # pos_summary, reporting_hf, map_confirmed_reporting
  "3_time.R", # global + HZ epicurves
  "3b_time_notification.R", # global + HZ epicurves by notification date
  "4_person.R", # age pyramid
  "5_delays.R", # delay
  "6_alert.R", # alerts overview + map
  "7_contact.R", # contact tracing overview + map
  "9_cfr.R", # létalité (CFR)
  "10_health-facilitiesf.R" # parcours de soins (structures visitées)
)

for (s in scripts) {
  message("\n>>> Running ", s)
  source(here::here("R", s))
}

# --- 2. Data cut-off date ---------------------------------------------------
# Modification time of the source positives summary = when the data were last
# refreshed. Passed to the report so the title page can stamp it.
date_updated <- butembo_pos$date_updated # source-file modification date (Date)

# --- 3. Render the .docx ----------------------------------------------------
message("\n>>> Rendering butembo-report.docx (cut-off ", date_updated, ")")
quarto::quarto_render(
  input = here::here("report", "butembo-report.qmd"),
  output_format = "docx",
  execute_params = list(date_updated = as.character(date_updated))
)

message(
  "\nDone -> ",
  here::here("butembo-report", "butembo-report.docx")
)
