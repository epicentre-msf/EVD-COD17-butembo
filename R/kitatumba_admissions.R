# Summarise CTE Kitatumba admissions and exits by period (Juil to Oct weeks).
# Reads the latest nominative export, writes the Word table to tables_dir.

#* TODO ------------------------------------
# - [ ]

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))

#* ADMISSION KITATUMBA ----------------------------------------------

latest_cte_kit_path <- fs::dir_ls(fs::path(
  butembo_project_data_path,
  'linelists/cte_kitatumba/export_nominatif'
)) |>
  max()

cte_kit_ll <- rpxl::rp_xlsb(
  latest_cte_kit_path,
  password = "Ambiguous nombrer PArtnershiP floAt APPArAgus {(135)}[790]",
  sheet = 1
) |>
  as_tibble()

last_week <- "S44"

# Monday after the last reported week, so later dates fall out of the periods
last_week_end <- lubridate::floor_date(
  as.Date("2026-01-04"),
  "week",
  week_start = CONFIG$week_start
) +
  7 * as.integer(sub("^S", "", last_week))

# July to September are calendar months, October is split by week
to_period <- function(d) {
  dplyr::case_when(
    d >= last_week_end ~ NA_character_,
    d >= as.Date("2026-07-01") & d < as.Date("2026-08-01") ~ "Juil",
    d >= as.Date("2026-08-01") & d < as.Date("2026-09-01") ~ "Août",
    d >= as.Date("2026-09-01") & d < as.Date("2026-10-01") ~ "Sept",
    d >= as.Date("2026-10-01") ~ paste0(
      "Oct S",
      lubridate::isoweek(
        lubridate::floor_date(d, "week", week_start = CONFIG$week_start)
      )
    )
  )
}

morgue_site <- "Morgue HGR Kitatumba | Kyangike | Butembo"

# morgue entries are not isolation admissions
cat(
  "Morgue rows dropped:",
  sum(cte_kit_ll$isolation_site_id %in% morgue_site),
  "of",
  nrow(cte_kit_ll),
  "\n"
)

kit_adm <- cte_kit_ll |>
  dplyr::filter(!isolation_site_id %in% morgue_site) |>
  dplyr::mutate(
    adm_period = to_period(as.Date(date_admission_eff)),
    exit_period = to_period(as.Date(date_exit_eff)),
    age_years = dplyr::case_match(
      age_unit,
      "Ans" ~ as.numeric(age),
      "Mois" ~ as.numeric(age) / 12,
      "Jour" ~ as.numeric(age) / 365.25
    ),
    exit_type = dplyr::case_when(
      type_of_exit == "" ~ NA_character_,
      type_of_exit == "Abandon" ~ "perdu_de_vue",
      type_of_exit == "Guéri" ~ "gueri",
      type_of_exit == "Retour a la maison (non cas)" ~ "sortie_non_cas",
      type_of_exit == "Décédé" & EVD_status == "Confirmé" ~ "deces_confirme",
      type_of_exit == "Décédé" & EVD_status == "Non cas" ~ "deces_non_cas",
      type_of_exit == "Décédé" ~ "deces_suspect"
    ),
    # same-day deaths count as under 24h
    death_delay = dplyr::case_when(
      exit_type != "deces_confirme" ~ NA_character_,
      as.Date(date_exit_eff) == as.Date(date_admission_eff) ~ "moins_24h",
      as.Date(date_exit_eff) > as.Date(date_admission_eff) ~ "plus_24h"
    )
  )

cat(
  "Rows kept:",
  nrow(kit_adm),
  "\n",
  "Admissions after",
  last_week,
  "(set to NA):",
  sum(as.Date(kit_adm$date_admission_eff) >= last_week_end, na.rm = TRUE),
  "\n",
  "Admissions outside Jul-Oct:",
  sum(is.na(kit_adm$adm_period)),
  "\n",
  "Exits outside Jul-Oct:",
  sum(!is.na(kit_adm$exit_type) & is.na(kit_adm$exit_period)),
  "\n",
  "Confirmed deaths without a delay:",
  sum(kit_adm$exit_type %in% "deces_confirme" & is.na(kit_adm$death_delay)),
  "\n"
)

dplyr::count(kit_adm, EVD_status, exit_type, death_delay)

#* Summary table data ----------------------------------------------------

# the current week is incomplete, so it stays out of the table
current_period <- to_period(Sys.Date())
# NA once today is past the last reported week, so nothing is dropped
current_period <- current_period[!is.na(current_period)]

cat(
  "Current week",
  current_period,
  "dropped: admissions",
  sum(kit_adm$adm_period %in% current_period),
  "| exits",
  sum(!is.na(kit_adm$exit_type) & kit_adm$exit_period %in% current_period),
  "\n"
)

period_levels <- c(
  "Juil",
  "Août",
  "Sept",
  sort(unique(grep(
    "^Oct S",
    c(kit_adm$adm_period, kit_adm$exit_period),
    value = TRUE
  )))
) |>
  setdiff(current_period)

adm_counts <- kit_adm |>
  dplyr::filter(!is.na(adm_period), !adm_period %in% current_period) |>
  dplyr::summarise(
    adm_total = dplyr::n(),
    adm_confirmes = sum(EVD_status == "Confirmé"),
    adm_suspects = adm_total - adm_confirmes,
    adm_moins_15 = sum(age_years < 15, na.rm = TRUE),
    .by = adm_period
  ) |>
  tidyr::pivot_longer(
    -adm_period,
    names_to = "row",
    values_to = "n"
  ) |>
  dplyr::rename(period = adm_period)

exit_counts <- kit_adm |>
  dplyr::filter(
    !is.na(exit_type),
    !is.na(exit_period),
    !exit_period %in% current_period
  ) |>
  dplyr::mutate(
    row = dplyr::if_else(
      exit_type == "deces_confirme",
      paste0(exit_type, "_", death_delay),
      exit_type
    )
  ) |>
  dplyr::count(period = exit_period, row)

# transferred has no category in type_of_exit, so it stays at zero
exit_rows <- c(
  "gueri",
  "sortie_non_cas",
  "transfere",
  "perdu_de_vue",
  "deces_confirme_moins_24h",
  "deces_confirme_plus_24h",
  "deces_non_cas",
  "deces_suspect"
)
adm_rows <- c("adm_confirmes", "adm_suspects", "adm_moins_15")

kit_grid <- tidyr::expand_grid(
  row = c(adm_rows, exit_rows),
  period = period_levels
)
n_grid <- nrow(kit_grid)

kit_grid <- kit_grid |>
  dplyr::left_join(
    dplyr::bind_rows(adm_counts, exit_counts),
    by = dplyr::join_by(row, period),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(n = dplyr::coalesce(n, 0L))

cat("Grid rows before / after join:", n_grid, "/", nrow(kit_grid), "\n")

kit_totals <- dplyr::bind_rows(
  adm_counts |> dplyr::filter(row == "adm_total"),
  kit_grid |>
    dplyr::filter(row %in% exit_rows) |>
    dplyr::summarise(n = sum(n), row = "sortie_total", .by = period)
)

row_labels <- c(
  adm_confirmes = "Admissions confirmées",
  adm_suspects = "Admissions suspects",
  adm_moins_15 = "Admissions < 15 ans",
  gueri = "Sorties : guéris",
  sortie_non_cas = "Sorties : non cas",
  transfere = "Sorties : transférés",
  perdu_de_vue = "Sorties : perdus de vue",
  deces_confirme_moins_24h = "Décès confirmés < 24 h",
  deces_confirme_plus_24h = "Décès confirmés > 24 h",
  deces_non_cas = "Décès non cas",
  deces_suspect = "Décès suspects",
  adm_total = "Total admissions",
  sortie_total = "Total sorties"
)

kit_summary <- dplyr::bind_rows(kit_grid, kit_totals) |>
  dplyr::mutate(
    row = factor(row, levels = names(row_labels), labels = row_labels),
    period = factor(period, levels = period_levels)
  ) |>
  tidyr::pivot_wider(names_from = period, values_from = n) |>
  dplyr::arrange(row) |>
  dplyr::mutate(Total = rowSums(dplyr::pick(dplyr::all_of(period_levels))))

kit_summary

#* Word table ------------------------------------------------------------

week_cols <- grep("^Oct S", period_levels, value = TRUE)

kit_ft_data <- kit_summary |>
  dplyr::mutate(
    groupe = dplyr::case_when(
      row %in% row_labels[adm_rows] ~ "Admissions",
      row %in% row_labels[exit_rows] ~ "Sorties",
      .default = "Totaux"
    ),
    row = as.character(row)
  ) |>
  dplyr::relocate(groupe)

total_rows <- which(kit_ft_data$groupe == "Totaux")

kit_ft <- flextable::flextable(kit_ft_data) |>
  flextable::set_header_labels(
    groupe = "",
    row = "",
    values = as.list(stats::setNames(sub("^Oct ", "", week_cols), week_cols))
  ) |>
  flextable::add_header_row(
    values = c("", "Mois", "Octobre", ""),
    colwidths = c(2, 3, length(week_cols), 1)
  ) |>
  flextable::merge_v(j = "groupe") |>
  flextable::theme_booktabs() |>
  flextable::fontsize(size = 9, part = "all") |>
  flextable::align(
    j = c(period_levels, "Total"),
    align = "center",
    part = "all"
  ) |>
  flextable::bold(part = "header") |>
  flextable::bold(i = total_rows) |>
  flextable::bold(j = "Total") |>
  flextable::bold(j = "groupe") |>
  flextable::valign(j = "groupe", valign = "top") |>
  flextable::hline(i = c(3, 11), border = officer::fp_border(width = 1)) |>
  flextable::autofit()

kit_ft

flextable::save_as_docx(
  kit_ft,
  path = fs::path(
    kit_flux_dir,
    paste0("kit_admissions_", last_week),
    ext = "docx"
  )
)
