#* CLEAN BUTEMBO DATA ------------------------------------------------------------------------------------

# Clean the linelist, transmission, alert and contact data
# Timestamped exports saved under Donnees/propre

# load all paths and libraries
source("R/0_global.R")

time_write <- time_stamp()

export_prefix <- "BUT-EVD"

#* Import data ------------------------------------------------------------
#! Check before running - which Health zone to include
FILTER_HZ <- c("Butembo", "Katwa", "Musienene")

# load the latest linelist
ll_narr <- rpxl::rp_xlsb(latest_narr_ll, password = "ebolaExport", sheet = 1) |>
  as_tibble() |>
  filter(adm2_comptabilisation %in% FILTER_HZ)

#* CLEAN DATA ------------------------------------------------------------
age_labs <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+")

ll_narr_clean <- ll_narr |>
  mutate(
    date_symptom_onset = ymd(date_symptom_onset),
    type_of_exit = case_when(
      type_of_exit == "" ~ "Actif",
      .default = type_of_exit
    ),
    type_of_exit = forcats::fct_relevel(
      type_of_exit,
      "Actif",
      "Guéri",
      "Abandon",
      "Décédé"
    ),
    infection_butembo = case_when(
      infection_butembo == "Musienene" ~ "Locale",
      infection_butembo == "Oui" ~ "Locale",
      infection_butembo == "Non" ~ "Importée",
      .default = "Incertaine"
    ),
    infection_butembo = factor(
      infection_butembo,
      levels = c("Incertaine", "Importée", "Locale"),
    ),
    # uniformise les codes de sexe
    sex = case_match(
      sex,
      c("H", "M") ~ "Homme",
      "F" ~ "Femme"
    ),
    sex = factor(sex, levels = c("Homme", "Femme")),
    age_group = cut(
      age,
      breaks = c(seq(0, 60, 10), Inf),
      right = FALSE,
      labels = age_labs
    ),
    across(where(is.character), ~ na_if(.x, "")),
    # split "site | aire de santé | zone de santé" into 3 columns
    adm3_isolation = str_squish(str_split_i(isolation_site_id, fixed("|"), 2)),
    adm2_isolation = str_squish(str_split_i(isolation_site_id, fixed("|"), -1)),
    isolation_site_id = str_squish(str_split_i(
      isolation_site_id,
      fixed("|"),
      1
    )),

    adm1_name = case_when(
      res_equal_onset == "Oui" ~ adm1_name__res,
      .default = adm1_name__onset
    ),
    adm2_name = case_when(
      res_equal_onset == "Oui" ~ adm2_name__res,
      .default = adm2_name__onset
    ),

    adm3_name = case_when(
      res_equal_onset == "Oui" ~ adm3_name__res,
      .default = adm3_name__onset
    )
  ) |>
  rename(pid = patient_site_id)

# source-file modification date, saved alongside the data
ll_narr_update <- as.Date(fs::file_info(latest_narr_ll)$modification_time)

time_write <- time_stamp()

export_prefix <- "BUT-EVD"
file_base <- glue::glue(
  "{export_prefix}_BUTEMBO_linelist__{time_write}"
)

saveRDS(
  list(
    data = ll_narr_clean,
    date_updated = ll_narr_update
  ),
  fs::path(
    butembo_project_data_path,
    "propre",
    paste0(file_base, ".rds")
  )
)

#* TRANSMISSION DATA ------------------------------------------------

trans_dat <- rio::import(transmission_path) |>
  as_tibble() |>
  filter(certainty %in% c("hypothesis", "known contact")) |>
  mutate(across(c(start_exposure, end_exposure), ~ ymd(.x))) |>
  rename(from = id_infector, to = id_infectee)

time_write <- time_stamp()

export_prefix <- "BUT-EVD"

file_base <- glue::glue(
  "{export_prefix}_BUTEMBO_transmission-data__{time_write}"
)

saveRDS(
  trans_dat,
  fs::path(
    butembo_project_data_path,
    "propre",
    paste0(file_base, ".rds")
  )
)

#* ALERTS DATABASE ----------------------------

# harmonise les noms d'aires de santé (adm3) des sitreps sur ceux du fond de carte
clean_adm3 <- function(x) {
  x <- str_to_sentence(str_squish(x))
  case_when(
    str_detect(x, regex("musayi", ignore_case = TRUE)) ~ "Maman Musayi",
    x %in% c("Wanama", "Wanamahi") ~ "Wanamahika",
    x == "Monde" ~ "Mondo",
    x %in% c("Kangike", "Yangike") ~ "Kyangike",
    # Misebere : nouvelle aire de santé rattachée à Kambuli
    x == "Misebere" ~ "Kambuli",
    .default = x
  )
}

alert <- rio::import(sitrep_path, which = "alert") |>
  as_tibble()

alert_clean <- alert |>
  mutate(
    sitrep_date = ymd(sitrep_date),
    across(
      c(contains("alert"), suspect_sampled, suspect_isolated),
      ~ as.numeric(str_squish(.x))
    ),
    across(
      c(contains("adm")),
      ~ str_squish(.x)
    ),
    adm3_name = clean_adm3(adm3_name)
  )

file_base <- glue::glue(
  "{export_prefix}_BUTEMBO_alert-data__{time_write}"
)

saveRDS(
  alert_clean,
  fs::path(
    butembo_project_data_path,
    "propre",
    paste0(file_base, ".rds")
  )
)

#* CONTACTS DATABASE ----------------------------

contact <- rio::import(sitrep_path, which = "contact") |>
  as_tibble()

contact_clean <- contact |>
  mutate(
    sitrep_date = ymd(sitrep_date),
    across(contains("contact"), ~ as.numeric(str_squish(.x))),
    across(
      c(contains("adm")),
      ~ str_squish(.x)
    ),
    adm3_name = clean_adm3(adm3_name)
  )


file_base <- glue::glue(
  "{export_prefix}_BUTEMBO_contact-data__{time_write}"
)

saveRDS(
  contact_clean,
  fs::path(
    butembo_project_data_path,
    "propre",
    paste0(file_base, ".rds")
  )
)
