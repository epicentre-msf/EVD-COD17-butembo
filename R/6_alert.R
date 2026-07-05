# ! Script of the ALERT situation in Butembo

source(here::here("R", "0_global.R"))
butembo_pos <- readRDS(latest_narr_ll_clean)
pos_data_clean <- butembo_pos$data
date_report <- butembo_pos$date_updated

#* Alerts -------------------------------------------------------

alert <- readRDS(latest_alert_clean) |>
  filter(sitrep_date >= "2026-06-21")

#* Alerts by HZ
alert_adm2 <- alert |>
  summarise(
    .by = c(adm2_name, sitrep_date),
    alert_new = sum(alert_new),
    alert_alive = sum(alert_alive),
    alert_dead = sum(alert_dead),
    alert_investigated = sum(alert_investigated),
    alert_validated = sum(alert_validated),
    suspect_sampled = sum(suspect_sampled),
    suspect_isolated = sum(suspect_isolated)
  )

#* Alerts by adm3
alert_adm3 <- alert |>
  summarise(
    .by = c(adm2_name, adm3_name),
    alert_new = sum(alert_new),
    alert_alive = sum(alert_alive),
    alert_dead = sum(alert_dead),
    alert_investigated = sum(alert_investigated),
    alert_validated = sum(alert_validated),
    suspect_sampled = sum(suspect_sampled),
    suspect_isolated = sum(suspect_isolated)
  )


#* GRAPH — new alerts by health zone over time ---------------------
# shared HZ palette, reused on the map below
adm2_cols <- c(
  "Butembo" = "#f08080", # salmon
  "Katwa" = "#3a7ca5" # blue
)

alert_hz_ts <- alert_adm2 |>
  arrange(sitrep_date) |>
  ggplot(aes(x = sitrep_date, y = alert_new, fill = adm2_name)) +
  geom_col(width = 1, alpha = .7, colour = "white") +
  scale_x_date(
    date_breaks = "1 days",
    date_minor_breaks = "1 day",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual("Zone de santé", values = adm2_cols, drop = FALSE) +
  labs(
    x = "Date de notification",
    y = "Nombre d'alertes",
    caption = paste0("Données au ", fr_date(date_report))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(
      colour = "grey60",
      linewidth = 0.2,
      linetype = "62"
    ),
    legend.position = "top",
    legend.justification = "left",
    plot.title = element_text(face = "bold", size = 13),
    plot.title.position = "plot",
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.x = element_line(colour = "grey70"),
    plot.margin = margin(10, 14, 10, 10)
  )

alert_hz_ts

ggsave(
  fs::path(out_dir, "butembo_alerts_by_hz_time.png"),
  alert_hz_ts,
  height = 7,
  width = 9,
  dpi = 300,
  bg = "white"
)

#* MAPS -------------------------------------

alerts_sf <- adm3 |>
  select(adm2_name, adm3_name) |>
  left_join(
    alert_adm3 |>
      select(adm2_name, adm3_name, n_alert = alert_new),
    by = join_by("adm2_name", "adm3_name")
  )

# interior points for areas with at least one alert
alert_pts <- alerts_sf |>
  filter(!is.na(n_alert) & n_alert > 0) |>
  st_point_on_surface()

tm_nk_alerts <- tm_basemap_epi() +
  # all health-area boundaries (incl. those with no alerts)
  tm_shape(adm3, bbox = st_bbox(adm3)) +
  tm_borders(col = "grey60", lwd = 1) +
  # health-zone boundaries on top
  tm_shape(adm2) +
  tm_borders(col = "grey20", lwd = 1.3) +
  # proportional circles at the centroids
  tm_shape(alert_pts) +
  tm_symbols(
    size = "n_alert",
    size.scale = tm_scale_continuous(
      values.scale = 2,
      values.range = c(0.4, 1)
    ),
    size.legend = tm_legend(title = "N alertes"),
    fill = "#ee9b00",
    fill_alpha = 0.8,
    col = "white",
    lwd = 0.5
  ) +
  tm_text("n_alert", size = 0.6, col = "white") +
  tm_theme_epi(
    date = zone_last_dates(alert),
    scalebar_breaks = c(0, 2, 5, 10)
  )

tmap_save(
  tm_nk_alerts,
  fs::path(out_dir, "butembo_map_alerts.png"),
  height = 8,
  width = 8,
  dpi = 300
)
