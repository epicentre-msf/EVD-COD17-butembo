# Butembo situation report — one-command render pipeline.

setwd(here::here())

# --- 1. Regenerate outputs --------------------------------------------------
# 0_butembo_prep_data.R refreshes the cleaned positives rds from SharePoint;
# the plotting scripts then read it and write PNGs to output/butembo/.
scripts <- c(
  "0_butembo_prep_data.R", # must run first: prepares the clean data
  "butembo_overview.R", # pos_summary, cfr, reporting_hf, map_confirmed_reporting
  "butembo_time.R.R", # global + HZ epicurves
  "butembo_person.R", # age pyramid
  "butembo_active.R", # active cases
  "butembo_alert.R", # alerts overview + map
  "butembo_contact.R", # contact tracing overview + map
  "butembo_delays.R" # delays from the epicentre LL
)

for (s in scripts) {
  message("\n>>> Running ", s)
  source(here::here("R", "butembo", s))
}

# --- 2. Data cut-off date ---------------------------------------------------
# Modification time of the source positives summary = when the data were last
# refreshed. Passed to the report so the title page can stamp it.
date_updated <- load_butembo_pos()$date_updated

# --- 3. Render the .docx ----------------------------------------------------
message("\n>>> Rendering butembo-report.docx (cut-off ", date_updated, ")")
quarto::quarto_render(
  input = here::here("butembo-report", "butembo-report.qmd"),
  output_format = "docx",
  execute_params = list(date_updated = as.character(date_updated))
)

message(
  "\nDone -> ",
  here::here("butembo-report", "butembo-report.docx")
)
