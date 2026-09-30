# Generates www/img/site_pin.png - the facility marker on the map
# (mod_map_place.R). Run manually after changing any constant below; the PNG
# is committed. Ported from evd-2026-app's dev/make_site_pin.R.
#
#   Rscript R/butembo_dashboard/dev/make_site_pin.R

r <- 1 # head radius
h <- 2.2 # tip -> head centre
n_arc <- 240
dot_r <- 0.40 * r

h_png <- 80L
outline_px <- 7
col_fill <- "#ffffff"
col_accent <- "#4a2422" # SITE_COL in mod_map_place.R

out <- "R/butembo_dashboard/www/img/site_pin.png"

theta <- acos(r / h)
a_from <- -pi / 2 + theta
a_to <- a_from + (2 * pi - 2 * theta)
arc <- seq(a_from, a_to, length.out = n_arc)
pin_x <- c(0, r * cos(arc))
pin_y <- c(0, h + r * sin(arc))

pad_px <- outline_px / 2
scale_px <- (h_png - 2 * pad_px) / (h + r)
w_png <- 2L * as.integer(ceiling((2 * r * scale_px + 2 * pad_px) / 2))
pad <- pad_px / scale_px

ragg::agg_png(
  out,
  width = w_png,
  height = h_png,
  units = "px",
  res = 96,
  background = "transparent"
)

graphics::par(
  mar = c(0, 0, 0, 0),
  xaxs = "i",
  yaxs = "i",
  ljoin = "round",
  lend = "round"
)
graphics::plot.new()
graphics::plot.window(
  xlim = c(-1, 1) * (w_png / scale_px) / 2,
  ylim = c(0 - pad, (h + r) + pad),
  asp = NA
)

graphics::polygon(pin_x, pin_y, col = col_fill, border = col_accent, lwd = outline_px)

dot_a <- seq(0, 2 * pi, length.out = n_arc)
graphics::polygon(
  dot_r * cos(dot_a),
  h + dot_r * sin(dot_a),
  col = col_accent,
  border = NA
)

grDevices::dev.off()
message("wrote ", out, " (", w_png, "x", h_png, ")")
