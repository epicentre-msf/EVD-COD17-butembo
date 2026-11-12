# Ad hoc analysis of Kayna for the Butembo EVD outbreak.
# Imports lab results; scratch script, not part of the pipeline.

#* TODO ------------------------------------
# - [ ] define the question to answer for Kayna
# - [ ] confirm lab file location and format

#* Load packages ---------------------------
source(here::here("R", "0_global.R"))

#* Path ------------------------------------

# lab_dir comes from 0_global.R; set the file once Hugo has named it
kayna_lab_path <- fs::path(lab_dir, "PartageResults_INRB_Kayna")
latest_kayna_lab <- fs::dir_ls(kayna_lab_path) |> max()

#* Import data -----------------------------

lab_kayna_raw <- rio::import(latest_kayna_lab) |>
  janitor::clean_names() |>
  as_tibble()

lab_kayna_clean <- lab_kayna_raw |>
  transmute(
    lab_id = id_labo_numero_de_l_echantillon,
    sex = sexe_homme_femme,
    age = as.numeric(str_replace_all(age_nombre, ",", ".")),
    age = if_else(age >= 100, NA, age),
    age = if_else(lab_id == "FHV-KAY-26-0246", 30, age),
    age_unit = tolower(str_squish(type_dage_ans_mois_jours)),
    age_unit = case_when(
      age_unit == "semaine(s)" ~ "semaine",
      age_unit == "missing" ~ NA_character_,
      .default = age_unit
    ),
    # same cuts as the dashboard; NA unit read as years, as in age_years()
    age_group = cut(
      floor(case_when(
        str_starts(age_unit, "mois") ~ age / 12,
        str_starts(age_unit, "semaine") ~ age / 52.18,
        str_starts(age_unit, "jour") ~ age / 365.25,
        .default = age
      )),
      breaks = CONFIG$age_breaks,
      right = FALSE,
      labels = CONFIG$age_labs
    ),
    first_sample = nouveau_prelevement_ou_reprelevement %in%
      c(
        "Nouveau Prélevement"
      ),
    patient_status = recode_values(
      statut_du_patient_au_moment_collecte_echantillon_dcd_vivant,
      c("Décédé") ~ "Décédé",
      "Vivant" ~ "Vivant",
      NA ~ NA_character_
    ),
    date_notification = harmonize_dates(date_de_notification_dd_mm_yyy),
    province,
    zone_de_sante = str_squish(str_to_sentence(zone_de_sante_debut_maladie)),
    zone_de_sante = recode_values(
      zone_de_sante,
      "Alibongo" ~ "Alimbongo",
      default = zone_de_sante
    ),
    aire_de_sante,
    origin = NA,
    sample_type = recode_values(
      type_d_echantillon,
      c("Ecouvillon Oral", "Ecouvillon Bucal(Oral,Salive)") ~ "Ecouvillon oral",
      c(
        "sang total",
        "Sang total",
        "Sérum",
        "Sang",
        "Cordon ombilical(Sang,Swab)"
      ) ~ "Sang total",
      c("Lait Matérnel") ~ "Lait Maternel",
      NA ~ NA_character_
    ),
    date_sampling = harmonize_dates(date_de_prelevement_dd_mm_yyy),
    date_lab_result = harmonize_dates(date_d_analyse_dd_mm_yyy),
    lab_result = case_when(
      resultat_final %in%
        c(
          "Negatif",
          "Negatit",
          "Négatif",
          "negatif",
          "Negatif MVE mais positif Rickettsia Salmonella",
          "Negatif MVE mais positif goutte epaisse"
        ) ~ "Négatif",
      resultat_final %in%
        c("Positf", "Positif") ~ "Positif",
      .default = NA_character_
    ),
    source = "INRB Kayna"
  ) |>
  distinct(lab_id, lab_result, .keep_all = TRUE)

#* Lab time series -------------------------

lab_pal <- c("Négatif" = "#3a9d8f", "Positif" = "#8b0000")

# Epicurve helpers copied from butembo_dashboard/global.R; sourcing it loads the app
label_epiweek <- function(x) {
  yr <- format(x, "%G")
  new_yr <- is.na(dplyr::lag(yr)) | yr != dplyr::lag(yr)
  paste0(
    "S",
    as.integer(format(x, "%V")),
    dplyr::if_else(new_yr, paste0("\n", yr), "")
  )
}

scale_x_epicurve <- function(width = "2 weeks") {
  scale_x_date(
    breaks = scales::breaks_width(width),
    labels = label_epiweek
  )
}

geom_epicurve <- function(...) {
  geom_col(col = "grey70", linewidth = 0.2, width = 7, ...)
}

theme_epicurve <- function() {
  theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.spacing.x = unit(1, "lines"),
      legend.position = "bottom"
    )
}

# Fades the latest week, which is still incomplete
scale_alpha_last <- function() {
  scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.5), guide = "none")
}

# k rescales a proportion onto the count axis so 100% meets the tallest bar
scale_y_epicurve <- function(k = NULL, prop_lab = NULL) {
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08)),
    labels = scales::label_comma(),
    sec.axis = if (!is.null(k)) {
      sec_axis(
        ~ . / k,
        name = prop_lab,
        breaks = seq(0, 1, by = 0.1),
        labels = scales::label_number(scale = 100)
      )
    } else {
      waiver()
    }
  )
}

lab_weeks <- lab_kayna_clean |>
  mutate(
    week = lubridate::floor_date(
      date_lab_result,
      unit = "week",
      week_start = CONFIG$week_start
    )
  )

# Only Positif and Négatif results are plotted, as in the dashboard
message(
  "Kayna lab: ",
  nrow(lab_weeks),
  " samples; ",
  sum(is.na(lab_weeks$week)),
  " dropped (no result date), ",
  sum(!is.na(lab_weeks$week) & is.na(lab_weeks$lab_result)),
  " dropped (result not Positif/Négatif)"
)

lab_tested <- lab_weeks |>
  filter(!is.na(week), !is.na(lab_result)) |>
  mutate(
    lab_result = factor(lab_result, levels = names(lab_pal)),
    sample_type = tidyr::replace_na(sample_type, "(Manquant)"),
    first_sample = factor(
      first_sample,
      levels = c(TRUE, FALSE),
      labels = c("Premier prélèvement", "Autre prélèvement")
    )
  )

# Sample-type colours as in the dashboard; first-sample colours are ad hoc
sample_type_pal <- c(
  "Sang total" = "#9e2a2b",
  "Ecouvillon oral" = "#6d85b6",
  "Lait Maternel" = "#e09f3e",
  "(Manquant)" = "#bdbdbd"
)
first_sample_pal <- c(
  "Premier prélèvement" = "#2a6f97",
  "Autre prélèvement" = "#bdbdbd"
)

# Proportion lines start at ISO week 31 of 2026, when testing was steady
line_start <- lubridate::floor_date(
  as.Date("2026-01-04"),
  unit = "week",
  week_start = 1
) +
  7 * (31 - 1)

# 1. By result
bars_result <- count(lab_tested, week, lab_result)
prop_pos <- lab_tested |>
  summarise(.by = week, prop = mean(lab_result == "Positif")) |>
  filter(week >= line_start)
k_result <- max(summarise(bars_result, .by = week, n = sum(n))$n)

p_lab_result <- bars_result |>
  ggplot(aes(week, n, fill = lab_result)) +
  geom_epicurve(aes(alpha = week == max(week))) +
  geom_line(
    data = prop_pos,
    aes(week, prop * k_result),
    inherit.aes = FALSE,
    colour = "grey15"
  ) +
  geom_point(
    data = prop_pos,
    aes(week, prop * k_result),
    inherit.aes = FALSE,
    colour = "grey15"
  ) +
  scale_alpha_last() +
  scale_fill_manual(values = lab_pal, name = "Résultat") +
  scale_x_epicurve() +
  scale_y_epicurve(k_result, "Positifs (%)") +
  labs(
    title = "INRB Kayna : résultats par semaine",
    subtitle = paste0(
      nrow(lab_tested),
      " échantillons testés, dont ",
      sum(lab_tested$lab_result == "Positif"),
      " positifs (",
      round(100 * mean(lab_tested$lab_result == "Positif")),
      " %)"
    ),
    x = "Semaine du résultat (ISO)",
    y = "Échantillons testés"
  ) +
  theme_epicurve()
print(p_lab_result)

# 2. By sample type
bars_type <- count(lab_tested, week, sample_type)
prop_blood <- lab_tested |>
  summarise(.by = week, prop = mean(sample_type == "Sang total")) |>
  filter(week >= line_start)
k_type <- max(summarise(bars_type, .by = week, n = sum(n))$n)

p_lab_type <- bars_type |>
  ggplot(aes(week, n, fill = sample_type)) +
  geom_epicurve(aes(alpha = week == max(week))) +
  geom_line(
    data = prop_blood,
    aes(week, prop * k_type),
    inherit.aes = FALSE,
    colour = "grey15"
  ) +
  geom_point(
    data = prop_blood,
    aes(week, prop * k_type),
    inherit.aes = FALSE,
    colour = "grey15"
  ) +
  scale_alpha_last() +
  scale_fill_manual(values = sample_type_pal, name = "Type d'échantillon") +
  scale_x_epicurve() +
  scale_y_epicurve(k_type, "Sang total (%)") +
  labs(
    title = "INRB Kayna : type d'échantillon par semaine",
    subtitle = paste0(
      nrow(lab_tested),
      " échantillons testés, dont ",
      sum(lab_tested$sample_type == "Sang total"),
      " de sang total (",
      round(100 * mean(lab_tested$sample_type == "Sang total")),
      " %)"
    ),
    x = "Semaine du résultat (ISO)",
    y = "Échantillons testés"
  ) +
  theme_epicurve()
print(p_lab_type)

# 3. By new sample vs other
p_lab_first <- count(lab_tested, week, first_sample) |>
  ggplot(aes(week, n, fill = first_sample)) +
  geom_epicurve(aes(alpha = week == max(week))) +
  scale_alpha_last() +
  scale_fill_manual(values = first_sample_pal, name = "Prélèvement") +
  scale_x_epicurve() +
  scale_y_epicurve() +
  labs(
    title = "INRB Kayna : premiers prélèvements par semaine",
    subtitle = paste0(
      nrow(lab_tested),
      " échantillons testés, dont ",
      sum(lab_tested$first_sample == "Premier prélèvement"),
      " premiers prélèvements (",
      round(100 * mean(lab_tested$first_sample == "Premier prélèvement")),
      " %)"
    ),
    x = "Semaine du résultat (ISO)",
    y = "Échantillons testés"
  ) +
  theme_epicurve()
print(p_lab_first)

# 4. By health zone
p_lab_zone <- lab_tested |>
  mutate(zone_de_sante = tidyr::replace_na(zone_de_sante, "(Manquant)")) |>
  count(week, zone_de_sante, lab_result) |>
  ggplot(aes(week, n, fill = lab_result)) +
  geom_epicurve() +
  facet_wrap(vars(zone_de_sante)) +
  scale_fill_manual(values = lab_pal, name = "Résultat") +
  scale_x_epicurve("6 weeks") +
  scale_y_epicurve() +
  labs(
    title = "INRB Kayna : résultats par zone de santé",
    subtitle = paste0(
      nrow(lab_tested),
      " échantillons testés dans ",
      dplyr::n_distinct(lab_tested$zone_de_sante),
      " zones de santé"
    ),
    x = "Semaine du résultat (ISO)",
    y = "Échantillons testés"
  ) +
  theme_epicurve()
print(p_lab_zone)

#* Save plots ------------------------------

ggsave(
  fs::path(kayna_lab_dir, "kayna_lab_result.png"),
  p_lab_result,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)
ggsave(
  fs::path(kayna_lab_dir, "kayna_lab_sample_type.png"),
  p_lab_type,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)
ggsave(
  fs::path(kayna_lab_dir, "kayna_lab_first_sample.png"),
  p_lab_first,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)
ggsave(
  fs::path(kayna_lab_dir, "kayna_lab_zone.png"),
  p_lab_zone,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

#* Comparison: INRB Béni ------------------

other_lab_files <- fs::dir_ls(
  local_lab_dir,
  regexp = paste0(CONFIG$export_prefix, "_lab_data__")
)
stopifnot("no saved lab_data rds" = length(other_lab_files) > 0)
other_lab_path <- other_lab_files[which.max(
  fs::file_info(other_lab_files)$modification_time
)]

lab_other_weeks <- readRDS(other_lab_path) |>
  filter(source == "INRB Béni") |>
  mutate(
    week = lubridate::floor_date(
      date_lab_result,
      unit = "week",
      week_start = CONFIG$week_start
    )
  )

message(
  "INRB Béni: ",
  nrow(lab_other_weeks),
  " samples; ",
  sum(is.na(lab_other_weeks$week)),
  " dropped (no result date), ",
  sum(
    !is.na(lab_other_weeks$week) &
      !lab_other_weeks$lab_result %in% names(lab_pal)
  ),
  " dropped (result not Positif/Négatif)"
)

lab_other_tested <- lab_other_weeks |>
  filter(!is.na(week), lab_result %in% names(lab_pal)) |>
  mutate(
    lab_result = factor(lab_result, levels = names(lab_pal)),
    sample_type = dplyr::if_else(
      sample_type == "Unknown",
      "(Manquant)",
      sample_type
    )
  )

bars_other <- count(lab_other_tested, week, lab_result)
prop_pos_other <- lab_other_tested |>
  summarise(.by = week, prop = mean(lab_result == "Positif"))
k_other <- max(summarise(bars_other, .by = week, n = sum(n))$n)

p_lab_other <- bars_other |>
  ggplot(aes(week, n, fill = lab_result)) +
  geom_epicurve(aes(alpha = week == max(week))) +
  geom_line(
    data = prop_pos_other,
    aes(week, prop * k_other),
    inherit.aes = FALSE,
    colour = "grey15"
  ) +
  geom_point(
    data = prop_pos_other,
    aes(week, prop * k_other),
    inherit.aes = FALSE,
    colour = "grey15"
  ) +
  scale_alpha_last() +
  scale_fill_manual(values = lab_pal, name = "Résultat") +
  scale_x_epicurve() +
  scale_y_epicurve(k_other, "Positifs (%)") +
  labs(
    title = "INRB Béni : résultats par semaine",
    subtitle = paste0(
      nrow(lab_other_tested),
      " échantillons testés, dont ",
      sum(lab_other_tested$lab_result == "Positif"),
      " positifs (",
      round(100 * mean(lab_other_tested$lab_result == "Positif")),
      " %)"
    ),
    x = "Semaine du résultat (ISO)",
    y = "Échantillons testés"
  ) +
  theme_epicurve()
print(p_lab_other)

ggsave(
  fs::path(kayna_lab_dir, "inrb_beni_lab_result.png"),
  p_lab_other,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)

# By sample type; proportion line runs from the first week, unlike Kayna
bars_type_other <- count(lab_other_tested, week, sample_type)
prop_blood_other <- lab_other_tested |>
  summarise(.by = week, prop = mean(sample_type == "Sang total"))
k_type_other <- max(summarise(bars_type_other, .by = week, n = sum(n))$n)

p_lab_type_other <- bars_type_other |>
  ggplot(aes(week, n, fill = sample_type)) +
  geom_epicurve(aes(alpha = week == max(week))) +
  geom_line(
    data = prop_blood_other,
    aes(week, prop * k_type_other),
    inherit.aes = FALSE,
    colour = "grey15"
  ) +
  geom_point(
    data = prop_blood_other,
    aes(week, prop * k_type_other),
    inherit.aes = FALSE,
    colour = "grey15"
  ) +
  scale_alpha_last() +
  scale_fill_manual(values = sample_type_pal, name = "Type d'échantillon") +
  scale_x_epicurve() +
  scale_y_epicurve(k_type_other, "Sang total (%)") +
  labs(
    title = "INRB Béni : type d'échantillon par semaine",
    subtitle = paste0(
      nrow(lab_other_tested),
      " échantillons testés, dont ",
      sum(lab_other_tested$sample_type == "Sang total"),
      " de sang total (",
      round(100 * mean(lab_other_tested$sample_type == "Sang total")),
      " %)"
    ),
    x = "Semaine du résultat (ISO)",
    y = "Échantillons testés"
  ) +
  theme_epicurve()
print(p_lab_type_other)

ggsave(
  fs::path(kayna_lab_dir, "inrb_beni_lab_sample_type.png"),
  p_lab_type_other,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)

#* Age pyramid of positives ----------------

# Initial letter only: the raw coding (M/F or Homme/Femme) is unchecked
lab_pos <- lab_kayna_clean |>
  filter(
    lab_result == "Positif" & first_sample | lab_id == "FHV-KAY-26-0376"
  ) |>
  mutate(
    sex = case_match(
      toupper(str_sub(sex, 1, 1)),
      c("H", "M") ~ "Homme",
      "F" ~ "Femme"
    )
  )

# Bars stay in counts so the labels are absolute; only the axis reads in %
n_pyramid <- sum(!is.na(lab_pos$age_group) & !is.na(lab_pos$sex))
pyramid_breaks <- function(lims) {
  pct <- pretty(c(0, max(abs(lims)) / n_pyramid))
  sort(unique(c(-pct, pct))) * n_pyramid
}

p_pyramid_pos <- epivis::plot_pyramid(
  lab_pos,
  age_col = age_group,
  gender_col = sex,
  gender_levels = c("Homme", "Femme"),
  make_age_groups = FALSE,
  show_data_labs = TRUE,
  add_missing_cap = FALSE,
  colours = c("#8fb8d8", "#f2c88f"),
  x_lab = "Groupe d'âge",
  y_lab = "Part des cas positifs (%)"
) +
  scale_y_continuous(
    breaks = pyramid_breaks,
    labels = \(x) scales::label_percent(accuracy = 1)(abs(x) / n_pyramid)
  ) +
  labs(
    title = "INRB Kayna : cas positifs par âge et sexe",
    subtitle = paste0(
      "Cas positifs : ",
      nrow(lab_pos),
      " ; femmes : ",
      round(100 * mean(lab_pos$sex == "Femme", na.rm = TRUE)),
      " %"
    ),
    caption = if (n_pyramid < nrow(lab_pos)) {
      paste0(
        "Données manquantes pour ",
        nrow(lab_pos) - n_pyramid,
        " observations (âge : ",
        sum(is.na(lab_pos$age_group)),
        ", sexe : ",
        sum(is.na(lab_pos$sex)),
        ")"
      )
    } else {
      "Aucune donnée manquante"
    }
  ) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.major.x = element_line(
      colour = "grey65",
      linewidth = 0.3,
      linetype = "dashed"
    )
  )
print(p_pyramid_pos)

ggsave(
  fs::path(kayna_lab_dir, "inrb_kayna_pyramid_positive.png"),
  p_pyramid_pos,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)
