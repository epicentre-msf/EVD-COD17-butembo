# Launch the epishiny.timeline module on the Butembo linelist
#
# Uses the official epishiny.timeline package (epicentre-msf/epishiny.timeline).
# Install with: remotes::install_github("epicentre-msf/epishiny.timeline")

source(here::here("R", "0_global.R"))
#remotes::install_github("epicentre-msf/epishiny.timeline")

library(epishiny.timeline)

ll <- readRDS(latest_narr_ll_clean)$data

# `...` go to timeline_server(): id_var stays the join key, name_var / pid_var
# feed the "Identifiant" picker. All structures + all cases show by default.
# Every timeline_server() argument is set explicitly below. Note the package
# defaults to English outcome labels, so the French values used in this linelist
# ("Guéri" / "Décédé" / "Homme") are passed in.
launch_timeline(
  ll,
  id_var = "patient_name",
  name_var = "patient_name",
  pid_var = "pid",
  age_var = "age",
  sex_var = "sex",
  date_onset = "date_symptom_onset",
  date_exit = "date_exit_eff",
  outcome_var = "type_of_exit",
  outcome_levels = c("Guéri", "Décédé"),
  male_values = c("Homme", "M", "Male", "H"),
  hf_name_pattern = "HF_name_visited",
  hf_start_pattern = "date_start_HF_visited",
  hf_end_pattern = "date_end_HF_visited",
  disease_col = "#f2b0b0",
  incubation_col = "#ffe6ccbb",
  onset_col = "darkred",
  recovered_col = "#add8e6",
  died_col = "#7f7f7f",
  recovered_value = "Guéri"
)
