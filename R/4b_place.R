# ! Script of the PLACE situation in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated # source-file modification date (Date)

#* PLACE (geographic distribution) ------------------------------------------

# confirmed cases par aire de santé (adm3), joined to the adm3 geometry
cases_sf <- adm3 |>
  left_join(
    pos_data_clean |>
      count(adm3_name__res, name = "n_conf"),
    by = join_by("adm3_name" == "adm3_name__res")
  ) |>
  select(adm3_name, n_conf) |>
  arrange(desc(n_conf))

# part des cas confirmés avec une aire de santé d'isolement connue (adm3)
n_total <- nrow(pos_data_clean)
n_located <- sum(!is.na(pos_data_clean$adm3_name__res))
pct_located <- round(100 * n_located / n_total)

map_caption <- glue::glue(
  "{n_located} ({pct_located}%) des {n_total} cas ont une information sur l'aire de santé de résidence"
)

#? 0. Carte de référence — zones de santé (adm2) et aires de santé (adm3) ----

tm_butembo_ref <- tm_basemap_epi() +
  tm_shape(adm3, bbox = st_bbox(adm3)) +
  tm_borders(col = "grey60", lwd = 1) +
  # étiquettes des aires de santé, placées dans le polygone
  tm_text(
    text = "adm3_name",
    size = 0.5,
    col = "grey40",
    shadow = TRUE,
    just = "top",
    ymod = -0,
    remove_overlap = TRUE
  ) +
  # limites et noms de zones de santé au-dessus
  tm_shape(adm2) +
  tm_borders(col = "grey20", lwd = 1.3) +
  tm_text(
    text = "adm2_name",
    size = 0.9,
    fontface = "bold",
    col = "grey20",
    shadow = TRUE,
    alpha = .7
  ) +
  tm_theme_epi(
    credits = "Zones de santé (adm2) et aires de santé (adm3)",
    date = date_report,
    scalebar_breaks = c(0, 5, 10)
  )

tmap_save(
  tm_butembo_ref,
  fs::path(out_dir, "butembo_map_reference.png"),
  height = 8,
  width = 8,
  dpi = 300
)

#? 1. Carte à points — cas confirmés par aire de santé de résidence ---------

# un point par aire de santé de résidence avec des cas
res_pts <- cases_sf |>
  filter(!is.na(n_conf)) |>
  st_point_on_surface()

tm_butembo_res <- tm_basemap_epi() +
  tm_shape(adm3, bbox = st_bbox(adm3)) +
  tm_borders(col = "grey60", lwd = 1) +
  # étiquettes des aires de santé, placées dans le polygone
  tm_text(
    text = "adm3_name",
    size = 0.5,
    col = "grey40",
    shadow = TRUE,
    just = "top",
    ymod = -0.35,
    remove_overlap = TRUE
  ) +
  # limites de zones de santé au-dessus
  tm_shape(adm2) +
  tm_borders(col = "grey20", lwd = 1.3) +
  tm_shape(res_pts) +
  tm_symbols(
    size = "n_conf",
    size.scale = tm_scale_continuous(values.scale = 1.2),
    size.legend = tm_legend("Cas confirmés"),
    fill = "#bb3e03",
    fill_alpha = 0.6,
    col = "white",
    lwd = 0.5
  ) +
  tm_theme_epi(
    credits = map_caption,
    date = date_report,
    scalebar_breaks = c(0, 5, 10)
  )

tmap_save(
  tm_butembo_res,
  fs::path(out_dir, "butembo_map_cases_residence.png"),
  height = 8,
  width = 8,
  dpi = 300
)

#? 2. Choroplèthe — délai début des symptômes → notification par aire de santé ----

# délai médian (jours) début des symptômes → notification par aire de santé
delay_sf <- adm3 |>
  left_join(
    pos_data_clean |>
      mutate(
        delay_ons_not = as.numeric(date_notification - date_symptom_onset)
      ) |>
      filter(!is.na(delay_ons_not)) |>
      summarise(
        delay_med = median(delay_ons_not),
        n_delay = n(),
        .by = adm3_name__res
      ),
    by = join_by("adm3_name" == "adm3_name__res")
  ) |>
  select(adm3_name, delay_med, n_delay)

# part des cas avec un délai début des symptômes → notification calculable
n_delay_valid <- sum(
  !is.na(pos_data_clean$date_notification - pos_data_clean$date_symptom_onset)
)
pct_delay <- round(100 * n_delay_valid / n_total)

delay_caption <- glue::glue(
  "Délai médian par aire de santé de résidence · {n_delay_valid} ({pct_delay}%) des {n_total} cas ont un délai calculable"
)

tm_butembo_delay <- tm_basemap_epi() +
  tm_shape(delay_sf, bbox = st_bbox(adm3)) +
  tm_polygons(
    fill = "delay_med",
    fill.scale = tm_scale_continuous(
      values = "brewer.yl_or_rd",
      value.na = NA,
      label.na = ""
    ),
    fill.legend = tm_legend("Délai médian (jours)"),
    fill_alpha = .6,
    col = "grey60",
    lwd = 1
  ) +
  # étiquettes des aires de santé, placées dans le polygone
  tm_text(
    text = "adm3_name",
    size = 0.5,
    col = "grey40",
    shadow = TRUE,
    just = "top",
    ymod = -0.35,
    remove_overlap = TRUE
  ) +
  # limites de zones de santé au-dessus
  tm_shape(adm2) +
  tm_borders(col = "grey20", lwd = 1.3) +
  # nombre de cas confirmés par aire de santé de résidence
  tm_shape(res_pts) +
  tm_symbols(
    size = "n_conf",
    size.scale = tm_scale_continuous(values.scale = 1.2),
    size.legend = tm_legend("Cas confirmés"),
    fill = "#bb3e03",
    fill_alpha = 0.9,
    col = "white",
    lwd = 0.5
  ) +
  tm_theme_epi(
    credits = delay_caption,
    date = date_report,
    scalebar_breaks = c(0, 5, 10)
  )

tmap_save(
  tm_butembo_delay,
  fs::path(out_dir, "butembo_map_delay_notification.png"),
  height = 8,
  width = 8,
  dpi = 300
)
