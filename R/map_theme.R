# Reusable tmap basemap + theme for EVD analyses (tmap v4)
# -------------------------------------------------------------------
# Minimal / epurated look, but cartographically complete: north arrow,
# scale bar and a caption, stacked together bottom-left.
#
# Usage:
#   tm_basemap_epi() + tm_shape(x) + tm_polygons("n") + tm_theme_epi()
#   tm_basemap_epi() + tm_shape(x) + tm_symbols(size = "n") + tm_theme_epi(credits = "...", scalebar_breaks = c(0, 5, 10))

# Standard basemap tiles for EVD maps — defined once so every map stays
# consistent and the provider can be swapped in a single place.
# Valid providers: see `rownames(maptiles::get_providers())` / leaflet-providers.
epi_basemap_tiles <- "CartoDB.VoyagerNoLabels"

# Cached basemap raster. The tiles are downloaded ONCE with maptiles and stored
# as a GeoTIFF, then reused as a static image on every render. This avoids
# tmap::tm_basemap()'s live per-render tile fetch — which crashes (segfaults)
# the R session — and keeps every map pixel-identical. local/ is git-ignored,
# so on a fresh checkout the cache is rebuilt from `region` on first use.
epi_basemap_path <- here::here("local", "basemap", "nk_basemap.tif")

# Download + cache the basemap for `region`. Run once; re-run to refresh the
# tiles, change provider/zoom, or rebuild the cache on a new machine.
download_epi_basemap <- function(
  region,
  path = epi_basemap_path,
  provider = epi_basemap_tiles,
  zoom = 12
) {
  fs::dir_create(fs::path_dir(path))
  tiles <- maptiles::get_tiles(
    region,
    provider = provider,
    zoom = zoom,
    crop = TRUE
  )
  terra::writeRaster(tiles, path, overwrite = TRUE)
  invisible(path)
}

# Basemap layer for tmap. Loads the cached raster (building it from `region` on
# first use if the cache is missing) and draws it as a static RGB image.
tm_basemap_epi <- function(region = adm3, path = epi_basemap_path) {
  if (!file.exists(path)) {
    download_epi_basemap(region, path = path)
  }
  tm_shape(terra::rast(path)) + tm_rgb()
}

tm_theme_epi <- function(
  credits = "© Médecins Sans Frontières",
  date = NULL,
  scalebar_breaks = NULL,
  compass = TRUE
) {
  # stamp the data validity date under the credits. A single date renders as
  # "Données au 23/06/2026"; a named vector of dates (name = health zone) renders
  # per zone, e.g. "Données au — Butembo : 28/06/2026 · Katwa : 01/07/2026".
  if (!is.null(date)) {
    if (length(date) > 1 || !is.null(names(date))) {
      parts <- paste0(names(date), " : ", fr_date(date))
      credits <- paste0(
        credits,
        "\nDonnées au — ",
        paste(parts, collapse = " · ")
      )
    } else {
      credits <- paste0(credits, "\nDonnées au ", fr_date(date))
    }
  }

  # scale bar: let tmap pick breaks unless supplied (NULL breaks errors in v4)
  sb <- if (is.null(scalebar_breaks)) {
    tm_scalebar(group_id = "bottom", text.size = 0.6)
  } else {
    tm_scalebar(breaks = scalebar_breaks, group_id = "bottom", text.size = 0.6)
  }

  # stack order within the "bottom" group (first added = top):
  # compass -> scale bar -> caption
  furniture <- NULL
  if (compass) {
    furniture <- tm_compass(type = "arrow", size = 1.5, group_id = "bottom")
  }
  furniture <- if (is.null(furniture)) sb else furniture + sb
  furniture <- furniture +
    tm_credits(credits, group_id = "bottom", size = 0.7, col = "grey20")

  furniture +
    # everything in one panel, bottom-left, on a translucent white background
    tm_components(
      "bottom",
      position = tm_pos_in(
        "left",
        "bottom",
        align.h = "left",
        align.v = "bottom"
      ),
      #frame = FALSE,
      #bg = TRUE,
      #bg.color = "white",
      #bg.alpha = 0.7
    ) +
    # epurated layout: no map frame, no legend frame, light margins
    tm_layout(
      frame = FALSE,
      inner.margins = c(0.02, 0.02, 0.02, 0.02),
      legend.position = tm_pos_in("right", "top", align.h = "right"),
      legend.frame = FALSE,
      legend.bg = FALSE,
      legend.title.size = 0.9,
      legend.text.size = 0.7,
      text.fontfamily = "sans"
    )
}
