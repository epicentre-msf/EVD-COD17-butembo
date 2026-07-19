# Launch the epishiny.timeline module on the Butembo linelist

source(here::here("R", "0_global.R"))
source(here::here("R", "epishiny_timeline.R"))

ll <- readRDS(latest_narr_ll_clean)$data

# `...` go to timeline_server(): id_var stays the join key, name_var / pid_var
# feed the "Identifiant" picker. All structures + all cases show by default.
launch_timeline(
  ll,
  name_var = "patient_name",
  pid_var = "pid"
)
