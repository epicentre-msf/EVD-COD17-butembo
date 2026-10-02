# Shared ggplot theme for the EVD-COD17 project.
# Single source of truth so every figure (lab, symptoms, ...) looks the same.
# Sourced by the analysis scripts.

# Combined theme: epitheme_gg base + the project's size / legend overrides.
# Returns a list so it can be added to any ggplot with `+ theme_evd()`.
#   - base_size / axis_title_size: forwarded to epithemes::epitheme_gg()
#   - legend_position: "top" (default), "bottom", "none", ...
#   - ...: any other epithemes::epitheme_gg() arg (e.g. strip_text_size)
theme_evd <- function(
  base_size = 12,
  axis_title_size = 14,
  legend_position = "top",
  ...
) {
  list(
    epithemes::epitheme_gg(
      base_size = base_size,
      axis_title_size = axis_title_size,
      ...
    ),
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 16),
      axis.title = ggplot2::element_text(size = 18),
      legend.text = ggplot2::element_text(size = 16),
      legend.position = legend_position,
      strip.text = ggplot2::element_text(size = 16, hjust = 0)
    )
  )
}
