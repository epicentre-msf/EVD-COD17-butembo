# ! Script of the CONTACT FOLLOW-UP situation in Butembo

source(here::here("R", "0_global.R"))
tmap::tmap_mode("plot")

#* Contact tracing ------------------------------------------
contact <- readRDS(latest_contact_clean) |>
  filter(sitrep_date >= "2026-06-21")

# Number contact to see
contact_adm2 <- contact |>
  summarise(
    .by = c(sitrep_date, adm2_name),
    contact_to_follow = sum(contact_to_follow),
    contact_seen = sum(contact_seen),
    contact_suspect = sum(contact_suspect),
    contact_exit = sum(contact_exit)
  ) |>
  mutate(
    contact_seen_pct = round(contact_seen / contact_to_follow * 100, digits = 2)
  )

contact_adm3 <- contact |>
  summarise(
    .by = c(sitrep_date, adm2_name, adm3_name),
    contact_to_follow = sum(contact_to_follow),
    contact_seen = sum(contact_seen),
    contact_suspect = sum(contact_suspect),
    contact_exit = sum(contact_exit)
  ) |>
  mutate(
    follow_rate = if_else(
      contact_to_follow > 0,
      100 * contact_seen / contact_to_follow,
      NA_real_
    )
  )


#* GRAPH — contacts to follow by health zone over time ---------------------
# shared HZ palette
adm2_cols <- c(
  "Butembo" = "#f08080", # salmon
  "Katwa" = "#3a7ca5" # blue
)

date_report <- max(contact_adm2$sitrep_date)

contact_adm2

contact_hz_ts <- ggplot() +
  geom_col(
    data = contact_adm2,
    aes(x = sitrep_date, y = contact_to_follow, fill = "Non vus"),
    width = .95,

    alpha = .55
  ) +
  geom_col(
    data = contact_adm2,
    aes(x = sitrep_date, y = contact_seen, fill = "Vus"),
    width = .95,
    alpha = .75,
  ) +
  facet_wrap(~adm2_name, nrow = 2) +
  scale_x_date(
    date_breaks = "1 days",
    date_minor_breaks = "1 day",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    "Cas contacts",
    values = c("Non vus" = "grey85", "Vus" = "seagreen")
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Date",
    y = "Nombre de contacts à suivre",
    caption = paste0("Données au ", fr_date(date_report))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 15, face = "bold"),
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

contact_hz_ts

ggsave(
  fs::path(out_dir, "butembo_contacts_by_hz_time.png"),
  contact_hz_ts,
  height = 10,
  width = 8,
  dpi = 300,
  bg = "white"
)
#* MAP LAST DAY CONTACT TRACING -------------------------------------------------

contact_sf <- adm3 |>
  left_join(
    contact_adm3 |>
      # each zone's own latest sitrep day (zones update on different dates)
      slice_max(sitrep_date, n = 1, by = adm2_name) |>
      select(
        adm2_name,
        adm3_name,
        contact_seen,
        contact_to_follow,
        contact_suspect,
        follow_rate
      ),
    by = join_by("adm2_name", "adm3_name")
  )

# interior points for areas with at least one suspect contact
contacts_pts <- contact_sf |>
  filter(!is.na(contact_to_follow) & contact_to_follow > 0) |>
  st_point_on_surface()

tm_nk_choro <- tm_basemap_epi() +
  tm_shape(contact_sf, bbox = st_bbox(adm3)) +
  tm_polygons(
    fill = "follow_rate",
    fill.scale = tm_scale_continuous(
      values = c("#FFE3B0", "#005f73"),
      limits = c(0, 100),
      value.na = "transparent"
    ),
    fill.legend = tm_legend(title = "Taux de suivi \ndes contacts (%)"),
    fill_alpha = 0.7,
    col = "grey60",
    lwd = 0.5
  ) +
  # health-zone boundaries on top
  tm_shape(adm2) +
  tm_borders(col = "grey20", lwd = 1.3) +
  # proportional bubbles for number of contacts to follow-up
  tm_shape(contacts_pts) +
  tm_symbols(
    size = "contact_to_follow",
    size.scale = tm_scale_continuous(
      values.scale = 3
    ),
    size.legend = tm_legend(
      title = "Contacts à suivre",
      position = tm_pos_in("right", "bottom", align.h = "right")
    ),
    fill = "#bb3e03",
    fill_alpha = 0.85,
    col = "white",
    lwd = 0.5
  ) +
  tm_text("contact_to_follow", size = 0.6, col = "white") +
  tm_theme_epi(
    date = zone_last_dates(contact),
    scalebar_breaks = c(0, 5, 10)
  )

tmap_save(
  tm_nk_choro,
  fs::path(out_dir, "butembo_map_contact.png"),
  height = 8,
  width = 8,
  dpi = 300
)
