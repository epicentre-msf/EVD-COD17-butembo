# Alert and contact script

#* ALERTS DATABASE -------------------------

# alert <- rio::import(sitrep_path, which = "alert") |>
#   as_tibble() |>
#   rename(
#     adm2_name = adm2_nom,
#     adm3_name = adm3_nom,
#     alert_new = alert_nouvelles,
#     alert_alive = alert_vivantes,
#     alert_dead = alert_decedee,
#     alert_investigated = alert_investiguees,
#     alert_validated = alert_validees,
#     suspect_sampled = suspect_preleves,
#     suspect_isolated = suspect_isoles
#   )

# alert_clean <- alert |>
#   mutate(
#     sitrep_date = harmonize_dates(sitrep_date),
#     across(
#       c(contains("alert"), suspect_sampled, suspect_isolated),
#       ~ as.numeric(str_squish(.x))
#     ),
#     across(
#       c(contains("adm")),
#       ~ str_squish(.x)
#     ),
#     adm3_name = clean_adm3(adm3_name)
#   ) |>
#   # sitreps before this date are incomplete
#   filter(sitrep_date >= CONFIG$sitrep_start)

# cli::cli_alert_info(
#   "alerts: {nrow(alert_clean)} rows kept of {nrow(alert)} \\
#    from {CONFIG$sitrep_start}"
# )

# export_clean(alert_clean, "alert-data", time_write)

# #* CONTACTS DATABASE -----------------------

# contact <- rio::import(sitrep_path, which = "contact") |>
#   as_tibble() |>
#   rename(
#     adm2_name = adm2_nom,
#     adm3_name = adm3_nom,
#     contact_new = contact_nouveaux,
#     contact_to_follow = contact_a_suivre,
#     contact_seen = contact_vus,
#     contact_suspect = contact_suspects,
#     contact_exit = contact_sortis_21
#   )

# contact_clean <- contact |>
#   mutate(
#     sitrep_date = harmonize_dates(sitrep_date),
#     across(contains("contact"), ~ as.numeric(str_squish(.x))),
#     across(
#       c(contains("adm")),
#       ~ str_squish(.x)
#     ),
#     adm3_name = clean_adm3(adm3_name)
#   ) |>
#   filter(sitrep_date >= CONFIG$sitrep_start)

# cli::cli_alert_info(
#   "contacts: {nrow(contact_clean)} rows kept of {nrow(contact)} \\
#    from {CONFIG$sitrep_start}"
# )

# export_clean(contact_clean, "contact-data", time_write)
